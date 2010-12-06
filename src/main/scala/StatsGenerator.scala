package surveys.StatsGenerator

import surveys.SurveyClasses._

case class Stats(mean: Double, dev: Double, med: Double, sample_size: Int, xs: List[Double]) {
  def correlationWith(s: Stats): Double = {
      assert(sample_size == s.sample_size)
      val values = xs.zip(s.xs)
      values.foldLeft(0: Double) {
        case (acc, (fst, snd)) => (fst-mean)*(snd-s.mean) + acc
      } / (values.size * dev * s.dev)
  }
}

object StatsGenerator {
  def stats[T: Numeric](xs: List[T]): Option[Stats] = xs match {
    case Nil => None
    case xs =>
    val xds = xs map { x => implicitly[Numeric[T]].toDouble(x) }
      val mean = xds.sum / xs.size
      val variance = xds.foldLeft(0: Double) {
        case (acc, x) => (x-mean)*(x-mean) + acc
      } / xs.size
      val med = xds.sorted.apply(xs.size / 2)
      Some(Stats(mean, scala.math.sqrt(variance), med, xs.size, xds))
  }

  def withDefaultStats(s: Option[Stats]) = {
    s getOrElse Stats(0.0, 0.0, 0.0, 0, Nil)
  }

  def getStats(xs: List[Answer]): Stats = {
    val optionStats = stats(xs map (_.value))
    withDefaultStats(optionStats)
  }

  def getCompleteStats(xs: List[Survey]): (Stats, Stats) = {
    val quality: List[Answer] = xs flatMap (_.values)
    val attendance: List[Int] = xs flatMap (_.attendance)
    (getStats(quality), withDefaultStats(stats(attendance)))
  }

  def getStatsByCriterium[T](xs: List[Survey], criterium: Survey => T): Map[T, (Stats, Stats)] = {
    val grouped: Map[T, List[Survey]] = xs groupBy criterium
    grouped mapValues getCompleteStats
  }

  def statsByClassType(xs: List[Survey]): Map[String, (Stats, Stats)] = {
    getStatsByCriterium(xs, _.clazz.code)
  }

  def statsByTitle(xs: List[Survey]): Map[String, (Stats, Stats)] = {
    getStatsByCriterium(xs, _.person.title)
  }

  def statsByPosition(xs: List[Survey]): Map[String, (Stats, Stats)] = {
    getStatsByCriterium(xs, _.person.position)
  }

  def statsByPersonSubject(xs: List[Survey]): Map[(Person, Subject), (Stats, Stats)] = {
    getStatsByCriterium(xs, x => (x.person, x.clazz.subject))
  }

  def statsByAggregatedPosition(xs: List[Survey]): Map[String, (Stats, Stats)] = {
    val map: Map[String, String] = (
      List("doktorant informatyka", "doktorant matematyka", "doktorant MISDoMP").map(_ -> "doktoranci" ) ++
      List("wykładowca", "starszy wykładowca", "docent").map(_ -> "pracownicy dydaktyczni") ++
      List("asystent", "adiunkt", "profesor nadzwyczajny", "profesor zwyczajny", "profesor wizytujący").map(_ -> "pracownicy naukowo-dydaktyczni")
    ).toMap
    getStatsByCriterium(xs, x => map getOrElse(x.person.position, "inne"))
  }

  def statsByQuestion(xs: List[Survey]): Map[String, Stats] = {
    val answers: List[(String, Answer)] = xs flatMap (x => (x.values map (y => (x.id, y))))
    val byQuestion: Map[String, List[(String, Answer)]] = answers groupBy (_._2.question.value)
    byQuestion mapValues {
      xs => getStats(xs.sortBy{_._1}.map{_._2})
    }
  }

  def statsByQuestionMatrix(xs: List[Survey]): PartialMatrix[(Stats, Stats)] = {
    def toMultiMap[T,U](xs: List[(T,U)]): Map[T, Set[U]] = xs.groupBy(_._1).toMap.mapValues(_.map(_._2).toSet)
    def statsByQuestion(q: String, xs: List[Survey]): Stats = {
      val answers = for (x <- xs; a <- x.values if a.question.value == q) yield a
      getStats(answers)
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

  def getCommentsForPersonSubject(xs: List[Survey], person: Person, subject: Subject): List[String] = {
    val xss = xs.filter(x => x.person == person && x.clazz.subject == subject)
    xss.collect{ case Survey(_, _, _, _, _, Some(s)) => s }
  }
}
