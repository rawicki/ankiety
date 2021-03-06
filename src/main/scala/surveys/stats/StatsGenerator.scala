package surveys.stats

import surveys.model._

case class Stats[T](of: T, mean: Double, dev: Double, med: Double, sample_size: Int, xs: List[Double]) {
  def correlationWith(s: Stats[_]): Double = {
      assert(sample_size == s.sample_size)
      val values = xs.zip(s.xs)
      values.foldLeft(0: Double) {
        case (acc, (fst, snd)) => (fst-mean)*(snd-s.mean) + acc
      } / (values.size * dev * s.dev)
  }
}

case class CompositeStats[T: Show](xs: List[Stats[T]]) {
  assert(xs != Nil)
  val flat = StatsGenerator.stats("Statystyki łączne", xs flatMap (_.xs)).get
  val mean = flat.mean
  val dev = flat.dev
  val sample_size = xs.map(_.sample_size).max
}

case class CompleteStats[T, U: Show](of: T, quality: CompositeStats[U], attendance: Stats[Question])

case class ClassInstance(person: Person, subject: Subject, classType: String)

object StatsGenerator {
  def stats[T: Numeric, U](of: U, xs: List[T]): Option[Stats[U]] = xs match {
    case Nil => None
    case xs =>
    val xds = xs map { x => implicitly[Numeric[T]].toDouble(x) }
      val mean = xds.sum / xs.size
      val variance = xds.foldLeft(0: Double) {
        case (acc, x) => (x-mean)*(x-mean) + acc
      } / xs.size
      val med = xds.sorted.apply(xs.size / 2)
      Some(Stats(of, mean, scala.math.sqrt(variance), med, xs.size, xds))
  }

  def withDefaultStats[T](s: Option[Stats[T]]): Stats[T] = {
    s getOrElse sys.error("needed empty stats")
  }

  def getStats[T](of: T, xs: List[Answer]): Stats[T] = {
    val optionStats = stats(of, xs map (_.value))
    withDefaultStats(optionStats)
  }

  def getCompleteStats[T, U: Show](xs: List[Survey], of: Answer => U): T => Option[CompleteStats[T, U]] = {
    val quality: Map[U, List[Answer]] = (xs flatMap (_.values)) groupBy of
    val attendance: List[Answer] = xs flatMap (_.attendance)
    val attendanceQuestion = {
      val set = attendance.map(_.qi.question).toSet
      assert(set.size <= 1, set)
      set.headOption
    }
    if (quality.isEmpty || attendanceQuestion.isEmpty) {
      _ => None
    } else {
      val compositeStats = CompositeStats((quality map { case (key, answers) => getStats(key, answers) }).toList)
      x => Some(CompleteStats(x, compositeStats, getStats(attendanceQuestion.get, attendance)))
    }
  }

  def getStatsByCriterium[T, U: Show](xs: List[Survey], criterium: Survey => T, of: Answer => U): List[CompleteStats[T, U]] = {
    val grouped: Map[T, List[Survey]] = xs groupBy criterium
    (grouped map {
      case (key, value) => getCompleteStats(value, of).apply(key)
    }).toList.flatten
  }

  def statsByClassType(xs: List[Survey]): List[CompleteStats[String, Question]] =
    getStatsByCriterium(xs, _.clazz.code, _.qi.question)

  def statsByTitle(xs: List[Survey]): List[CompleteStats[String, Question]] =
    getStatsByCriterium(xs, _.person.title getOrElse "(brak)", _.qi.question)

  def statsByPosition(xs: List[Survey]): List[CompleteStats[String, Question]] =
    getStatsByCriterium(xs, _.person.position, _.qi.question)

  def statsByPersonSubject(xs: List[Survey]): List[CompleteStats[ClassInstance, QuestionInstance]] =
    getStatsByCriterium(xs, x => ClassInstance(x.person, x.clazz.subject, x.clazz.code), _.qi)

  def statsByAggregatedPosition(xs: List[Survey]): List[CompleteStats[String, QuestionInstance]] = {
    val map: Map[String, String] = (
      List("doktorant informatyka", "doktorant matematyka", "doktorant MISDoMP").map(_ -> "doktoranci" ) ++
      List("wykładowca", "starszy wykładowca", "docent").map(_ -> "pracownicy dydaktyczni") ++
      List("asystent", "adiunkt", "profesor nadzwyczajny", "profesor zwyczajny", "profesor wizytujący").map(_ -> "pracownicy naukowo-dydaktyczni")
    ).toMap
    getStatsByCriterium(xs, x => map getOrElse(x.person.position, "inne"), _.qi)
  }

  def statsByQuestion(xs: List[Survey]): CompositeStats[Question] = {
    val answers: List[(String, Answer)] = xs flatMap (x => (x.values map (y => (x.id, y))))
    import scala.collection.immutable.SortedMap
    def toSortedMap[A: Ordering, B](xs: Map[A,B]): SortedMap[A,B] = SortedMap.empty[A,B] ++ xs
    implicit val qo = Ordering.by[Question, String](_.value)
    val byQuestion: SortedMap[Question, List[(String, Answer)]] =
      toSortedMap(answers groupBy (_._2.qi.question))
    CompositeStats((byQuestion map {
      case (key, xs) => getStats(key, xs.sortBy{_._1}.map{_._2})
    }).toList)
  }

  def statsByQuestionMatrix(xs: List[Survey], qo: Ordering[Question]): PartialMatrix[(Stats[_], Stats[_])] = {
    def toMultiMap[T,U](xs: List[(T,U)]): Map[T, Set[U]] = xs.groupBy(_._1).toMap.mapValues(_.map(_._2).toSet)
    def statsByQuestion(q: Question, xs: List[Survey]): Stats[String] = {
      val answers = for (x <- xs; a <- x.values if a.qi.question == q) yield a
      getStats(q.value, answers)
    }
    def printInfo(q1: Question, q2: Question, surveys: List[Survey]): Unit = if (surveys.size < 100) {
      val N = 10
      print(q1.value); print(", "); println(q2.value)
      println("Stats based on %1d surveys, took %2d examples of subjects:".format(surveys.size, N))
      val subjects: Set[String] = surveys.map(_.clazz.subject.description).toSet take N
      println(subjects); println();
    }
    val answersByQuestion: Map[Question, Set[Survey]] = toMultiMap(for {
      as <- xs
      a <- as.values
    } yield (a.qi.question, as))
    val questions = answersByQuestion.keys.toList.sorted(qo)
    val values = for {
      q1 <- questions
      q2 <- questions
      val surveys = (answersByQuestion(q1) intersect answersByQuestion(q2)).toList
      if surveys != Nil
    } yield {
      (q1.value, q2.value) -> (statsByQuestion(q1, surveys), statsByQuestion(q2, surveys))
    }
    PartialMatrix(questions.map(_.value), values.toMap)
  }

  def getCommentsForPersonSubject(xs: List[Survey], person: Person, subject: Subject): List[(Class, String)] = {
    val xss = xs.filter(x => x.person == person && x.clazz.subject == subject)
    xss.collect{ case Survey(_, clazz, _, _, _, Some(s)) => (clazz, s) }
  }
}
