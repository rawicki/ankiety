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

  def personMeanAndDevation(xs: List[Answers]): Map[Person, Map[String, Stats]] = {
    val perPerson: Map[Person, List[Answers]] = xs groupBy (_.person)
    perPerson mapValues { xs =>
      val answers: List[Answer] = xs flatMap (_.values)
      val questions: Map[String, List[Answer]] = answers groupBy (_.question.value)
      questions mapValues getStats
    }
  }

  def statsByClassType(xs: List[Answers]): Map[String, (Stats, Stats)] = {
    val perPerson: Map[String, List[Answers]] = xs groupBy (_.clazz.code)
    perPerson mapValues { xs =>
      val answers: List[Answer] = xs flatMap (_.values)
      val (quality, attendance) = answers partition (_.question.value.startsWith("Na ilu"))
      (getStats(quality), getStats(attendance))
    }
  }

  def statsByTitle(xs: List[Answers]): Map[String, (Stats, Stats)] = {
    val perPerson: Map[String, List[Answers]] = xs groupBy (_.person.title)
    perPerson mapValues { xs =>
      val answers: List[Answer] = xs flatMap (_.values)
      val (quality, attendance) = answers partition (_.question.value.startsWith("Na ilu"))
      (getStats(quality), getStats(attendance))
    }
  }

  def statsByQuestion(xs: List[Answers]): Map[String, Stats] = {
    val answers: List[(String, Answer)] = xs flatMap (x => (x.values map (y => (x.id, y))))
    val byQuestion: Map[String, List[(String, Answer)]] = answers groupBy (_._2.question.value)
    byQuestion mapValues {
      xs => getStats(xs.sortBy{_._1}.map{_._2})
    }
  }

  def statsByPersonSubject(xs: List[Answers]): Map[(String, String), (Stats, Stats)] = {
    def to_str(p: Person): String = {
        p.title + " " + p.name + " " + p.lastName
    }
    val byPersonSubject = xs groupBy (x => (to_str(x.person), x.clazz.subject.description))
    byPersonSubject mapValues { x =>
      val xs = x flatMap (_.values)
      val (quality, attendance) = xs partition (_.question.value.startsWith("Na ilu"))
      (getStats(quality), getStats(attendance))
    }
  }
}
