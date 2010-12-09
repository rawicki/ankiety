import java.io.{OutputStreamWriter, FileOutputStream}
import scala.xml._

import surveys.SurveyClasses._
import surveys.DataImporter.DataImporter
import surveys.StatsGenerator.{Stats, CompleteStats, ClassInstance, StatsGenerator}

object GenerateReport {
  var next_tag_id: Int = 1

  def show_mean(s: Stats): NodeSeq =
      <span style="white-space: nowrap">
        <span title={s.name}>{show_double(s.mean)}</span>
        <span style="font-size: 0.7em" title="Odchylenie standardowe">(&sigma;: {show_double(s.dev)})</span>
      </span>

  def show_double(d: Double): String =
      "%2.3f" format d

  def dumpForSparkbar(s: Stats, domain: Seq[Int]): NodeSeq =
      <span class="inlinesparkbar">{
          val grouped = s.xs.groupBy(identity) mapValues (_.size)
          (for (x <- domain) yield grouped.getOrElse(x, 0)).mkString(",")
      }</span>

  def show_question_stats(s: Stats): NodeSeq =
      show_mean(s) ++ dumpForSparkbar(s, 1 to 7)

  def show_attendance_stats(s: Stats): NodeSeq =
      show_mean(s) ++ dumpForSparkbar(s, 5 to 95 by 10)

  def show_comments(comments: List[(String, String)]): NodeSeq =
    Seq(<div class="comments">{
      for((classType, comment) <- comments) yield Seq(
        <div class="comment"><div class="comment-header">{classType}</div>{ comment }</div>
      )
    }</div>)

  def show_comments_link(comments: List[(String, String)], id: String): NodeSeq =
    if(comments.nonEmpty){
        <a href="#" onClick={ "$(\"#comments-" ++ id ++ "\").toggle(); return(false);" }>
          Pokaż({ comments.size })
        </a>
    } else { new Text("Brak") }

  def showPartialMatrix[T](m: PartialMatrix[T], default: NodeSeq, triangleOnly: Boolean = false)(f: T => NodeSeq): NodeSeq = {
    <table>
      <tbody>
        <tr>
          <td>&nbsp;</td>
          {
            for ((_, index) <- m.labels.zipWithIndex) yield <th style="font-size: 0.6em">{"(" + index + ")"}</th>
          }
        </tr>
        {
          for ((label1, index1) <- m.labels.zipWithIndex) yield
            <tr>
              <th>{label1} <span style="font-size: 0.6em">({index1})</span></th>
              {
                for ((label2, index2) <- m.labels.zipWithIndex) yield
                  if (index1 < index2 && triangleOnly)
                    new Text(" ")
                  else
                    <td style="padding: 4px; white-space: nowrap;">{m.values.get(label1 -> label2).map(f).getOrElse(default)}</td>
              }
            </tr>
        }
      </tbody>
    </table>
  }

  def scatterPlot(data: List[(Double, Double)]): NodeSeq = {
    <div id="placeholder" style="width:600px;height:300px;margin: auto;"></div>
    <script id="source" language="javascript" type="text/javascript">
      $(function () {{
        var d = {
          (data map { case (x, y) => "[%1s, %2s]".format(x.toString, y.toString) }).mkString("[", ",", "]")
        };
        $.plot($({Unparsed("\"#placeholder\"")}), [
            {{
                data: d,
                points: {{show: true}}
            }}]);
      }});
    </script>
  }

  def getUniqueId(): Int = {
    next_tag_id = next_tag_id + 1
    next_tag_id
  }

  def main(args: Array[String]){
    val salt = if (args contains "md5") Some((1 to 10).map(_ => scala.util.Random.nextPrintableChar).mkString("")) else None
    salt foreach { x =>
      val fw = new OutputStreamWriter(new FileOutputStream("salt_KEEP_SECRET.txt"), "UTF-8")
      fw.write(x.toString)
      fw.close()
    }
    val answers = (new DataImporter(salt)).readSurveys
    val statsByQuestion = StatsGenerator.statsByQuestion(answers)
    val statsByClassType = StatsGenerator.statsByClassType(answers).sortBy(-_.quality.mean)
    val statsByTitle = StatsGenerator.statsByTitle(answers).sortBy(-_.quality.mean)
    val statsByPosition = StatsGenerator.statsByPosition(answers).sortBy(-_.quality.mean)
    val statsByAggregatedPosition = StatsGenerator.statsByAggregatedPosition(answers).sortBy(-_.quality.mean)
    val statsByPersonSubject = StatsGenerator.statsByPersonSubject(answers).sortBy(-_.quality.mean)
    val (quality, relativeFilled) = (for {
           CompleteStats(ClassInstance(person, subject, _), qualityStats, _) <- statsByPersonSubject
      val surveys = answers.filter(x => x.clazz.subject == subject && x.person == person)
      val questions = (for (x <- surveys; answer <- x.values) yield answer.question).toSet
      val ratios = for (q <- questions) yield (q.stats.filled: Double) / q.stats.allowed * 100
    } yield (qualityStats.mean, ratios.max)).unzip

    val statsByQuestionMatrix = StatsGenerator.statsByQuestionMatrix(answers)
    def show_per_category_stats(xs: List[CompleteStats[String]], category: String): NodeSeq =
      <table>
        <thead>
          <tr>
            <th>{ category }</th>
            <th>Pytania</th>
            <th>Obecności (%)</th>
            <th>Ile próbek</th>
          </tr>
        </thead>
        <tbody>
            {
              for (CompleteStats(label, quality, attendance) <- xs) yield
                <tr>
                  <td>{ label }</td>
                  <td>{ show_question_stats(quality) }</td>
                  <td>{ show_attendance_stats(attendance) }</td>
                  <td>{ attendance.sample_size }</td>
                </tr>
            }
        </tbody>
      </table>
    def show_per_person_stats(xs: List[CompleteStats[ClassInstance]]): NodeSeq =
      <table>
        <thead>
          <tr>
            <th>Osoba</th>
            <th>Przedmiot</th>
            <th>Typ</th>
            <th>Oceny</th>
            <th>Obecność (%)</th>
            <th>Próbka</th>
            <th>Komentarze</th>
          </tr>
        </thead>
        <tbody>
          {
            for (CompleteStats(classInstance @ ClassInstance(person, subject, classType), quality, attendance) <- xs) yield {
              val comments = StatsGenerator.getCommentsForPersonSubject(answers, classInstance)
              val comments_block_id = getUniqueId.toString
              <tr>
                <th>{ person }</th>
                <td>{ subject }</td>
                <td>{ classType }</td>
                <td>{ show_question_stats(quality) }</td>
                <td>{ show_attendance_stats(attendance) }</td>
                <td>{ attendance.sample_size }</td>
                <td>{ show_comments_link(comments, comments_block_id) }</td>
              </tr>
              <tr class="comments" id={ "comments-" ++ comments_block_id }>
                <td colspan="6">
                  { show_comments(comments) }
                </td>
              </tr>
            }
          }
        </tbody>
      </table>
    val report =
      <html>
        <head>
          <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
          <link rel="stylesheet" type="text/css" href="templates/style.css"/>
          <script type="text/javascript" src="templates/jquery-1.4.3.js"></script>
          <script type="text/javascript" src="templates/jquery.sparkline.js"></script>
          <script type="text/javascript" src="templates/flot/jquery.flot.js"></script>

          <script type="text/javascript">
              $(function() {{
                  /* Use 'html' instead of an array of values to pass options
                  to a sparkline with data in the tag */
                  $('.inlinesparkbar').sparkline('html', {{type: 'bar', barColor: 'blue'}});
              }});
          </script>
        </head>
        <body>
          <!-- github ribbon -->
          <a href="http://github.com/rawicki/ankiety"><img style="position: absolute; top: 0; left: 0; border: 0;" src="http://s3.amazonaws.com/github/ribbons/forkme_left_green_007200.png" alt="Fork me on GitHub" /></a>
          <h1>Wyniki ankiet 2009Z</h1>
          <h3>(wypełnionych ankiet: {answers.size})</h3>
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
                  for(stats <- statsByQuestion.xs.sortBy(_.mean)) yield
                    <tr>
                    <th>{ stats.name }</th>
                    <td>{ show_mean(stats) }</td>
                    </tr>
                }
              </tbody>
            </table>
          </div>
          <div class="center">
            <h2>Korelacja pomiędzy wynikami z pytań</h2>
            {
              showPartialMatrix(statsByQuestionMatrix, new Text("-"), true) {
                case (stats1, stats2) => {
                  def getColour(correlation: Double): String = {
                    (scala.math.min(400 - correlation * 400, 220)).toInt.toHexString * 3
                  }
                  val cor = stats1 correlationWith stats2
                  <span style={ "color: #" + getColour(cor) }>{show_double(cor)}</span>
                }
              }
            }
          </div>
          <div class="center">
            <h2>Średni wynik dla wszystkich pytań wg stopnia lub tytułu naukowego</h2>
            { show_per_category_stats(statsByTitle, "Stopień/Tytuł") }
          </div>
          <div class="center">
            <h2>Średni wynik dla wszystkich pytań wg rodzaju stanowiska</h2>
            { show_per_category_stats(statsByAggregatedPosition, "Rodzaj stanowiska") }
          </div>
          <div class="center">
            <h2>Średni wynik dla wszystkich pytań wg stanowiska</h2>
            { show_per_category_stats(statsByPosition, "Stanowisko") }
          </div>
          <div class="center">
            <h2>Średni wynik dla wszystkich pytań wg typu zajęć</h2>
            { show_per_category_stats(statsByClassType, "Typ zajęć") }
          </div>
          <div class="center">
            <h2>15 najlepszych wyników (osoba, przedmiot)</h2>
            { show_per_person_stats(statsByPersonSubject take 15) }
          </div>
          <div class="center">
            <h2>15 najgorszych wyników (osoba, przedmiot)</h2>
            { show_per_person_stats(statsByPersonSubject.reverse take 15) }
          </div>
          <div class="center">
            <h2>15 najbardziej kontrowersyjnych wyników (osoba, przedmiot)</h2>
            { show_per_person_stats(statsByPersonSubject.sortBy(-_.quality.dev) take 15) }
          </div>
          <div class="center">
            <h2>15 najczęściej opuszczanych zajęć (osoba, przedmiot)</h2>
            { show_per_person_stats(statsByPersonSubject.sortBy(_.attendance.mean) take 15) }
          </div>
          <div class="center">
            <h2>Ocena prowadzącego a procent wypełnionych ankiet</h2>
            { scatterPlot(quality zip relativeFilled) }
          </div>
        </body>
      </html>
    val fw = new OutputStreamWriter(new FileOutputStream("Report.html"), "UTF-8")
    fw.write(report.toString)
    fw.close()
  }
}
