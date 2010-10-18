import surveys.SurveyClasses._
import surveys.DataImporter.DataImporter
import surveys.HTMLGenerator.HTMLGenerator

object GenerateReport {
  def personMeanAndDevation(xs: List[Answers]): Map[Person, Map[String, (Double, Double, Int)]] = {
      def meanDevationAndSampleSize(xs: List[Int]): (Double, Double, Int) = {
          val mean = (xs.sum: Double) / xs.size
          val variance = xs.foldLeft(0: Double) {
              case (acc, x) => ((x-mean)*(x-mean): Double)/xs.size + acc
          }
          (mean, scala.math.sqrt(variance), xs.size)
      }
      val perPerson: Map[Person, List[Answers]] = xs groupBy (_.person)
      perPerson mapValues { xs =>
          val answers: List[Answer] = xs flatMap (_.values)
          val questions: Map[String, List[Answer]] = answers groupBy (_.question.value)
          questions mapValues (xs => meanDevationAndSampleSize(xs map (_.value)))
      }
  }

  def main(args: Array[String]){
    val answers = DataImporter.readAnswers
		val xs: List[(Person, String, (Double, Double, Int))] = for {
			(person, qs) <- personMeanAndDevation(answers).toList
			(question, (mean, devation, size)) <- qs.toList
		} yield (person, question, (mean, devation, size))
        val xss = xs filter {
            case (_, question, (_, _, _)) => question.startsWith("Og")
        }
		val byDevation = xss sortBy {
			case (_, _, (mean, devation, _)) => -mean
		}
		for ((person, question, (mean, devation, size)) <- byDevation take 30) {
			print(person)
            print(" " * (45 - person.toString().length))
			println("Mean: %1f,\tDev: %2f \t(%d)\tand Question '%3s'".format(mean, devation, size, question))
		}

    HTMLGenerator.write_report(Map("body_text" -> "ble!"))
  }
}
