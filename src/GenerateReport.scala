import surveys.SurveyClasses._
import surveys.DataImporter.DataImporter
import surveys.HTMLGenerator.HTMLGenerator

object GenerateReport {
  def personMeanAndDevation(xs: List[Answers]): Map[Person, Map[Question, (Double, Double)]] = {
      def meanAndDevation(xs: List[Int]): (Double, Double) = {
          val mean = (xs.sum: Double) / xs.size
          val variance = xs.foldLeft(0: Double) {
              case (acc, x) => ((x-mean)*(x-mean): Double)/xs.size + acc
          }
          (mean, scala.math.sqrt(variance))
      }
      val perPerson: Map[Person, List[Answers]] = xs groupBy (_.person)
      perPerson mapValues { xs =>
          val questions: Map[Question, List[Answer]] = (for (x <- xs; answer <- x.values) yield answer) groupBy (_.question)
          questions mapValues (xs => meanAndDevation(xs map (_.value)))
      }
  }

  def main(args: Array[String]){
    val answers = DataImporter.readAnswers
		val xs: List[(Person, Question, (Double, Double))] = for {
			(person, qs) <- personMeanAndDevation(answers).toList
			(question, (mean, devation)) <- qs.toList
		} yield (person, question, (mean, devation))
        val xss = xs filter {
            case (_, question, (_, _)) => !question.value.startsWith("Na ilu ")
        }
		val byDevation = xss sortBy {
			case (_, _, (mean, devation)) => -mean
		}
		for ((person, question, (mean, devation)) <- byDevation) {
			print(person)
            print(" " * (45 - person.toString().length))
			println("Mean: %1f,\tDev: %2f and Question '%3s'".format(mean, devation, question))
		}

    HTMLGenerator.write_report(Map("body_text" -> "ble!"))
  }
}
