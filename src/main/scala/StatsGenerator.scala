package surveys.StatsGenerator

import surveys.SurveyClasses._

case class Stats(mean: Double, dev: Double, med: Double, sample_size: Int, xs: List[Int]){
  def correlationWith(s: Stats): Double = {
      val values = xs.zip(s.xs)
      values.foldLeft(0: Double) {
        case (acc, (fst, snd)) => (fst-mean)*(snd-s.mean) + acc
      } / (values.size * dev * s.dev)
  }
}

object StatsGenerator {
  def getStats(xss: List[Answer]): Stats = {
    val xs = xss map (_.value)
    val mean = (xs.sum: Double) / xs.size
    val variance = xs.foldLeft(0: Double) {
      case (acc, x) => ((x-mean)*(x-mean): Double) + acc
    }/xs.size
    val med = xs.sorted.apply(xs.size / 2)
    Stats(mean, scala.math.sqrt(variance), med, xs.size, xs)
  }

  def personMeanAndDevation(xs: List[Survey]): Map[Person, Map[String, Stats]] = {
    val perPerson: Map[Person, List[Survey]] = xs groupBy (_.person)
    perPerson mapValues { xs =>
      val answers: List[Answer] = xs flatMap (_.values)
      val questions: Map[String, List[Answer]] = answers groupBy (_.question.value)
      questions mapValues getStats
    }
  }

  def statsByClassType(xs: List[Survey]): Map[String, (Stats, Stats)] = {
    val perPerson: Map[String, List[Survey]] = xs groupBy (_.clazz.code)
    perPerson mapValues { xs =>
      val answers: List[Answer] = xs flatMap (_.values)
      val (quality, attendance) = answers partition (_.question.value.startsWith("Na ilu"))
      (getStats(quality), getStats(attendance))
    }
  }

  def statsByTitle(xs: List[Survey]): Map[String, (Stats, Stats)] = {
    val perPerson: Map[String, List[Survey]] = xs groupBy (_.person.title)
    perPerson mapValues { xs =>
      val answers: List[Answer] = xs flatMap (_.values)
      val (quality, attendance) = answers partition (_.question.value.startsWith("Na ilu"))
      (getStats(quality), getStats(attendance))
    }
  }

  def statsByPosition(xs: List[Survey]): Map[String, (Stats, Stats)] = {
    val perPerson: Map[String, List[Survey]] = xs groupBy (_.person.position)
    perPerson mapValues { xs =>
      val answers: List[Answer] = xs flatMap (_.values)
      val (quality, attendance) = answers partition (_.question.value.startsWith("Na ilu"))
      (getStats(quality), getStats(attendance))
    }
  }

  def statsByAggregatedPosition(xs: List[Survey]): Map[String, (Stats, Stats)] = {
    var map: Map[String, String] = Map()
    List("doktorant informatyka", "doktorant matematyka") foreach { map += _ -> "doktoranci" }
    List("wykładowca", "starszy wykładowca", "docent") foreach { map += _ -> "pracownicy dydaktyczni" }
    List("asystent", "adiunkt", "profesor nadzwyczajny", "profesor zwyczajny", "profesor wizytujący") foreach { map += _ -> "pracownicy naukowo-dydaktyczni" }
    val perPerson: Map[String, List[Survey]] = xs groupBy (x => try map apply x.person.position catch { case _ => "inne" })
    perPerson mapValues { xs =>
      val answers: List[Answer] = xs flatMap (_.values)
      val (quality, attendance) = answers partition (_.question.value.startsWith("Na ilu"))
      (getStats(quality), getStats(attendance))
    }
  }

  def statsByQuestion(xs: List[Survey]): Map[String, Stats] = {
    val answers: List[(String, Answer)] = xs flatMap (x => (x.values map (y => (x.id, y))))
    val byQuestion: Map[String, List[(String, Answer)]] = answers groupBy (_._2.question.value)
    byQuestion mapValues {
      xs => getStats(xs.sortBy{_._1}.map{_._2})
    }
  }

  def statsByPersonSubject(xs: List[Survey]): Map[(Person, Subject), (Stats, Stats)] = {
    val byPersonSubject = xs groupBy (x => (x.person, x.clazz.subject))
    byPersonSubject mapValues { x =>
      val xs = x flatMap (_.values)
      val (quality, attendance) = xs partition (_.question.value.startsWith("Na ilu"))
      (getStats(quality), getStats(attendance))
    }
  }

  def getCommentsForPersonSubject(xs: List[Survey], person: Person, subject: Subject): List[String] = {
    val xss = xs.filter(x => x.person == person && x.clazz.subject == subject)
    xss.collect{ case Survey(_, _, _, _, Some(s)) => s }
  }
}
