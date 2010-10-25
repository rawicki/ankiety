import java.io.{OutputStreamWriter, FileOutputStream}

import surveys.SurveyClasses._
import surveys.DataImporter.DataImporter

case class Stats(mean: Double, dev: Double, med: Double, sample_size: Int, xs: List[Int]){
  def correlationWith(s: Stats): Double = {
      val values = xs.zip(s.xs)
      values.foldLeft(0: Double) {
        case (acc, (fst, snd)) => (fst-mean)*(snd-s.mean) + acc
      } / (values.size * dev * s.dev)
  }
}

object GenerateReport {
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

  def main(args: Array[String]){
		import scala.xml._
    def show_mean(s: Stats): NodeSeq = {
        Seq(new Text(show_double(s.mean) + " "), <span style="font-size: 0.7em; white-space: nowrap">(dev: {show_double(s.dev)})</span>)
    }
    def show_double(d: Double): String = {
        "%2.3f" format d
    }
    def dumpForSparkbar(s: Stats, domain: Seq[Int]): NodeSeq =
        <span class="inlinesparkbar">{
            val grouped = s.xs.groupBy(identity) mapValues (_.size)
            (for (x <- domain) yield grouped.getOrElse(x, 0)).mkString(",")
        }</span>
    def show_question_stats(s: Stats): NodeSeq =
        show_mean(s) + dumpForSparkbar(s, 1 to 7)
    def show_attendance_stats(s: Stats): NodeSeq =
        show_mean(s) + dumpForSparkbar(s, 5 to 95 by 10)

    val answers = DataImporter.readAnswers
    val fw = new OutputStreamWriter(new FileOutputStream("Report.html"), "UTF-8")

    val statsByQuestion = GenerateReport.statsByQuestion(answers).toList
    val statsByClassType = GenerateReport.statsByClassType(answers).toList.sortBy(-_._2._2.mean)
    val statsByTitle = GenerateReport.statsByTitle(answers).toList.sortBy(-_._2._2.mean).filter(_._2._1.sample_size > 50)
    val statsByPersonSubject = GenerateReport.statsByPersonSubject(answers).toList.sortBy(-_._2._2.mean).filter(_._2._1.sample_size > 7)
    val report =
      <html>
        <head>
          <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
          <link rel="stylesheet" type="text/css" href="templates/style.css"/>

					<script type="text/javascript" src="templates/jquery-1.4.3.js"></script>
			    <script type="text/javascript" src="templates/jquery.sparkline.js"></script>

			    <script type="text/javascript">
				    $(function() {{
				        /* Use 'html' instead of an array of values to pass options
				        to a sparkline with data in the tag */
				        $('.inlinesparkbar').sparkline('html', {{type: 'bar', barColor: 'blue'}});
				    }});
			    </script>
        </head>
        <body>
          <h1>Wyniki ankiet 2009Z</h1>
          <div class="center">
            <h2>Średni wynik wg pytania</h2>
            <table>
              <thead>
                <tr>
                  <th>Pytanie</th>
                  <th>Średnia</th>
                </tr>
              </thead>
              <tbody>
                {
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
            <h2>Korelacja pomiędzy wynikami z pytań</h2>
            <table>
              <tbody>
                {
                  for((label1, stats1) <- statsByQuestion.sortBy(_._2.mean)) yield {
                    <tr>
                      <th>{ label1 }</th>
                      {
                        for((label2, stats2) <- statsByQuestion.sortBy(_._2.mean)) yield {
                          <td style="padding: 4px; white-space: nowrap;">{ show_double(stats1 correlationWith stats2) }</td>
                        }
                      }
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
                      <td>{ show_question_stats(questions) }</td>
                    }
                  }
                </tr>
                <tr>
                  <td>Obecności</td>
                  {
                    for((_, (attendance, questions)) <- statsByTitle) yield {
                      <td>{ show_attendance_stats(attendance) }</td>
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
            <h2>Sredni wynik dla wszystkich pytań wg. typu zajęć</h2>
            <table>
              <thead>
                <tr>
                  <th>X</th>
                    {
                      for((label, _) <- statsByClassType) yield {
                        <th>{ label }</th>
                      }
                    }
                  </tr>
              </thead>
              <tbody>
                <tr>
                  <td>Pytania</td>
                  {
                    for((_, (attendance, questions)) <- statsByClassType) yield {
                      <td>{ show_question_stats(questions) }</td>
                    }
                  }
                </tr>
                <tr>
                  <td>Obecności</td>
                  {
                    for((_, (attendance, questions)) <- statsByClassType) yield {
                      <td>{ show_attendance_stats(attendance) }</td>
                    }
                  }
                </tr>
                <tr>
                  <td>Ile próbek</td>
                  {
                    for((_, (attendance, questions)) <- statsByClassType) yield {
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
                      <td>{ show_mean(questions) }</td>
                      <td>{ show_mean(attendance) }</td>
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
                      <td>{ show_question_stats(questions) }</td>
                      <td>{ show_attendance_stats(attendance) }</td>
                      <td>{ attendance.sample_size }</td>
                    </tr>
                  }
                }
              </tbody>
            </table>
          </div>
          <div class="center">
            <h2>15 najbardziej kontrowersyjnych wyników (osoba, przedmiot)</h2>
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
                  for(((person, subject), (attendance, questions)) <- statsByPersonSubject.sortBy(-_._2._2.dev) take 15) yield {
                    <tr>
                      <th>{ person }</th>
                      <td>{ subject }</td>
                      <td>{ show_question_stats(questions) }</td>
                      <td>{ show_attendance_stats(attendance) }</td>
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
