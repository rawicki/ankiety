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

  def main(args: Array[String]){
    def show_mean(s: Stats): String = {
        "%2.3f (dev: %2.3f)" format(s.mean, s.dev)
    }
    val answers = DataImporter.readAnswers
    val fw = new FileWriter("Report.html")

    val statsByTitle = GenerateReport.statsByTitle(answers).toList.sortBy(-_._2._2.mean).filter(_._2._1.sample_size > 50)
    val statsByPersonSubject = GenerateReport.statsByPersonSubject(answers).toList.sortBy(-_._2._2.mean).filter(_._2._1.sample_size > 7)
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
                  val statsByQuestion = GenerateReport.statsByQuestion(answers).toList
                  for((label, stats) <- statsByQuestion.sortBy(_._2.mean)) yield {
                    <tr>
                    <th>{ label }</th>
                    <td>{ show_mean(stats) }</td>
                    </tr>
                  }
                }
              </tbody>
            </table>
          </div>
          <div class="center">
            <h2>Sredni wynik dla wszystkich pytań wg. tytułu</h2>
            <table>
              <thead>
                <tr>
                  <th>X</th>
                    {
                      for((label, _) <- statsByTitle) yield {
                        <th>{ label }</th>
                      }
                    }
                  </tr>
              </thead>
              <tbody>
                <tr>
                  <td>Pytania</td>
                  {
                    for((_, (attendance, questions)) <- statsByTitle) yield {
                      <td>{ show_mean(questions) }</td>
                    }
                  }
                </tr>
                <tr>
                  <td>Obecności</td>
                  {
                    for((_, (attendance, questions)) <- statsByTitle) yield {
                      <td>{ show_mean(attendance) }</td>
                    }
                  }
                </tr>
                <tr>
                  <td>Ile próbek</td>
                  {
                    for((_, (attendance, questions)) <- statsByTitle) yield {
                      <td>{ attendance.sample_size }</td>
                    }
                  }
                </tr>
              </tbody>
            </table>
          </div>
          <div class="center">
            <h2>15 najlepszych wyników (osoba, przedmiot)</h2>
            <table>
              <thead>
                <tr>
                  <th>Osoba</th>
                  <th>Przedmiot</th>
                  <th>Oceny</th>
                  <th>Obecność</th>
                  <th>Próbka</th>
                </tr>
              </thead>
              <tbody>
                {
                  for(((person, subject), (attendance, questions)) <- statsByPersonSubject take 15) yield {
                    <tr>
                      <th>{ person }</th>
                      <td>{ subject }</td>
                      <td>{ show_mean(attendance) }</td>
                      <td>{ show_mean(questions) }</td>
                      <td>{ attendance.sample_size }</td>
                    </tr>
                  }
                }
              </tbody>
            </table>
          </div>
          <div class="center">
            <h2>15 najgorszych wyników (osoba, przedmiot)</h2>
            <table>
              <thead>
                <tr>
                  <th>Osoba</th>
                  <th>Przedmiot</th>
                  <th>Oceny</th>
                  <th>Obecność</th>
                  <th>Próbka</th>
                </tr>
              </thead>
              <tbody>
                {
                  for(((person, subject), (attendance, questions)) <- statsByPersonSubject.reverse take 15) yield {
                    <tr>
                      <th>{ person }</th>
                      <td>{ subject }</td>
                      <td>{ show_mean(attendance) }</td>
                      <td>{ show_mean(questions) }</td>
                      <td>{ attendance.sample_size }</td>
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
