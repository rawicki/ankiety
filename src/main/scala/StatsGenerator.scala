package surveys.StatsGenerator

import surveys.SurveyClasses._

case class Stats(name: String, mean: Double, dev: Double, med: Double, sample_size: Int, xs: List[Double]) {
  def correlationWith(s: Stats): Double = {
      assert(sample_size == s.sample_size)
      val values = xs.zip(s.xs)
      values.foldLeft(0: Double) {
        case (acc, (fst, snd)) => (fst-mean)*(snd-s.mean) + acc
      } / (values.size * dev * s.dev)
  }
}

case class CompositeStats(xs: List[Stats]) {
  assert(xs != Nil)
  val flat = StatsGenerator.stats(xs map (_.name) mkString ", ", xs flatMap (_.xs)).get
  val mean = flat.mean
  val dev = flat.dev
  val sample_size = xs.map(_.sample_size).max
}

case class CompleteStats[T](of: T, quality: CompositeStats, attendance: Stats)

case class ClassInstance(person: Person, subject: Subject, classType: String)

object StatsGenerator {
  def stats[T: Numeric](name: String, xs: List[T]): Option[Stats] = xs match {
    case Nil => None
    case xs =>
    val xds = xs map { x => implicitly[Numeric[T]].toDouble(x) }
      val mean = xds.sum / xs.size
      val variance = xds.foldLeft(0: Double) {
        case (acc, x) => (x-mean)*(x-mean) + acc
      } / xs.size
      val med = xds.sorted.apply(xs.size / 2)
      Some(Stats(name, mean, scala.math.sqrt(variance), med, xs.size, xds))
  }

  def withDefaultStats(s: Option[Stats]) = {
    s getOrElse Stats("(empty)", 0.0, 0.0, 0.0, 0, Nil)
  }

  def getStats(name: String, xs: List[Answer]): Stats = {
    val optionStats = stats(name, xs map (_.value))
    withDefaultStats(optionStats)
  }

  def getCompleteStats[T](xs: List[Survey]): T => CompleteStats[T] = {
    val quality: Map[String, List[Answer]] = (xs flatMap (_.values)) groupBy (_.question.value)
    val attendance: List[Int] = xs flatMap (_.attendance)
    val compositeStats = CompositeStats((quality map { case (key, answers) => getStats(key, answers) }).toList)
    CompleteStats(_, compositeStats, withDefaultStats(stats("Obecność", attendance)))
  }

  def getStatsByCriterium[T](xs: List[Survey], criterium: Survey => T): List[CompleteStats[T]] = {
    val grouped: Map[T, List[Survey]] = xs groupBy criterium
    (grouped map {
      case (key, value) => getCompleteStats(value)(key)
    }).toList
  }

  def statsByClassType(xs: List[Survey]): List[CompleteStats[String]] = getStatsByCriterium(xs, _.clazz.code)

  def statsByTitle(xs: List[Survey]): List[CompleteStats[String]] = getStatsByCriterium(xs, _.person.title)

  def statsByPosition(xs: List[Survey]): List[CompleteStats[String]] = getStatsByCriterium(xs, _.person.position)

  def statsByPersonSubject(xs: List[Survey]): List[CompleteStats[ClassInstance]] =
    getStatsByCriterium(xs, x => ClassInstance(x.person, x.clazz.subject, x.clazz.code))

  def statsByAggregatedPosition(xs: List[Survey]): List[CompleteStats[String]] = {
    val map: Map[String, String] = (
      List("doktorant informatyka", "doktorant matematyka", "doktorant MISDoMP").map(_ -> "doktoranci" ) ++
      List("wykładowca", "starszy wykładowca", "docent").map(_ -> "pracownicy dydaktyczni") ++
      List("asystent", "adiunkt", "profesor nadzwyczajny", "profesor zwyczajny", "profesor wizytujący").map(_ -> "pracownicy naukowo-dydaktyczni")
    ).toMap
    getStatsByCriterium(xs, x => map getOrElse(x.person.position, "inne"))
  }

  def statsByQuestion(xs: List[Survey]): CompositeStats = {
    val answers: List[(String, Answer)] = xs flatMap (x => (x.values map (y => (x.id, y))))
    val byQuestion: Map[String, List[(String, Answer)]] = answers groupBy (_._2.question.value)
    CompositeStats((byQuestion map {
      case (key, xs) => getStats(key, xs.sortBy{_._1}.map{_._2})
    }).toList)
  }

  def statsByQuestionMatrix(xs: List[Survey]): PartialMatrix[(Stats, Stats)] = {
    def toMultiMap[T,U](xs: List[(T,U)]): Map[T, Set[U]] = xs.groupBy(_._1).toMap.mapValues(_.map(_._2).toSet)
    def statsByQuestion(q: String, xs: List[Survey]): Stats = {
      val answers = for (x <- xs; a <- x.values if a.question.value == q) yield a
      getStats(q, answers)
    }
    val answersByQuestion: Map[String, Set[Survey]] = toMultiMap(for {
      as <- xs
      a <- as.values
    } yield (a.question.value, as))
    val questions = answersByQuestion.keys.toList
    val values = for {
      q1 <- questions
      q2 <- questions
      val surveys = (answersByQuestion(q1) intersect answersByQuestion(q2)).toList
      if surveys != Nil
    } yield (q1, q2) -> (statsByQuestion(q1, surveys), statsByQuestion(q2, surveys))
    PartialMatrix(questions, values.toMap)
  }

  def getCommentsForPersonSubject(xs: List[Survey], classInstance: ClassInstance): List[(String, String)] = {
    val xss = xs.filter(x => x.person == classInstance.person && x.clazz.subject == classInstance.subject)
    xss.collect{ case Survey(_, clazz, _, _, _, Some(s)) => (clazz.code, s) }
  }
}
