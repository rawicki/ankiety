import surveys.SurveyClasses._
import surveys.DataImporter.DataImporter
import surveys.HTMLGenerator.HTMLGenerator

object GenerateReport {
  case class Stats(mean: Double, dev: Double, med: Double, sample_size: Int)

  def getStats(xss: List[Answer]): Stats = {
      val xs = xss map (_.value)
      val mean = (xs.sum: Double) / xs.size
      val variance = xs.foldLeft(0: Double) {
          case (acc, x) => ((x-mean)*(x-mean): Double)/xs.size + acc
      }
      val med = xs.sorted.apply(xs.size / 2)
      Stats(mean, scala.math.sqrt(variance), med, xs.size)
  }

  def personMeanAndDevation(xs: List[Answers]): Map[Person, Map[String, Stats]] = {
      val perPerson: Map[Person, List[Answers]] = xs groupBy (_.person)
      perPerson mapValues { xs =>
          val answers: List[Answer] = xs flatMap (_.values)
          val questions: Map[String, List[Answer]] = answers groupBy (_.question.value)
          questions mapValues getStats
      }
  }

  def main(args: Array[String]){
    val answers = DataImporter.readAnswers
		val xs: List[(Person, String, Stats)] = for {
			(person, qs) <- personMeanAndDevation(answers).toList
			(question, stats) <- qs.toList
		} yield (person, question, stats)
        val xss = xs filter {
            case (person, question, _) => person.name.startsWith("Szy") && person.lastName.startsWith("Ac")
        }
		val byDevation = xss sortBy {
			case (_, _, Stats(mean, devation, _, _)) => -mean
		}
		for ((person, question, Stats(mean, devation, med, size)) <- byDevation take 30) {
			print(person)
            print(" " * (45 - person.toString().length))
			println("Mean: %1f,\tDev: %2f \t(%d)\tand Question '%3s'".format(mean, devation, size, question))
		}

    HTMLGenerator.write_report(Map("body_text" -> "ble!"))
  }
}
