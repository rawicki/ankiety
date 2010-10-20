import java.io.FileWriter

import surveys.SurveyClasses._
import surveys.DataImporter.DataImporter

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

  def statsByTitle(xs: List[Answers]): Map[String, (Stats, Stats)] = {
      val perPerson: Map[String, List[Answers]] = xs groupBy (_.person.title)
      perPerson mapValues { xs =>
          val answers: List[Answer] = xs flatMap (_.values)
          val (quality, attendance) = answers partition (_.question.value.startsWith("Na ilu"))
          (getStats(quality), getStats(attendance))
      }
  }

  def statsByQuestion(xs: List[Answers]): Map[String, Stats] = {
      val answers: List[Answer] = xs flatMap (_.values)
      val byQuestion: Map[String, List[Answer]] = answers groupBy (_.question.value)
      byQuestion mapValues getStats
  }

  def main(args: Array[String]){
    val answers = DataImporter.readAnswers
    val statsByTitle = GenerateReport.statsByTitle(answers)
    for((title, stats) <- statsByTitle) {
      print(title)
      print("\t")
      println(stats)
    }
    val fw = new FileWriter("Report.html")

    val report =
      <html>
        <head>
          <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
          <link rel="stylesheet" type="text/css" href="templates/style.css"/>
        </head>
        <body>
          <h1>Wyniki ankiet 2009Z</h1>
          <div class="center">
            <h2>Średni wynik wg pytania</h2>
            <table>
              <thead>
                <th>Pytanie</th>
                <th>Średnia</th>
              </thead>
              <tbody>
                {
                  val statsByQuestion = GenerateReport.statsByQuestion(answers)
                  for((label, stats) <- statsByQuestion) yield {
                    <tr>
                    <th>{ label }</th>
                    <td>{ stats.mean }</td>
                    </tr>
                  }
                }
              </tbody>
            </table>
          </div>
        </body>
      </html>
    fw.write(report.toString)
    fw.close()
  }
}
