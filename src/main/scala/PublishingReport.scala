package surveys.ReportBuilder

import scala.xml._

import surveys.SurveyClasses._
import surveys.StatsGenerator.{Stats, CompleteStats, CompositeStats, ClassInstance, StatsGenerator}
import surveys.SubjectCategories.{Category, Categorization}

object PublishingReport extends Report {
  
  def show_per_category_stats[T: Show](xs: List[CompleteStats[String, T]], category: String): NodeSeq =
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
                <td>{ quality.sample_size }</td>
              </tr>
          }
      </tbody>
    </table>
  
  def show_per_person_stats_rows(xs: List[ClassStats], comments: ClassInstance => List[(Class, String)]): NodeSeq = {
    val understandingQuestion = Question("Czy zajęcia były prowadzone w sposób zrozumiały?")
    (for (CompleteStats(classInstance @ ClassInstance(person, subject, classType), quality, attendance) <- xs) yield {
      val cs = comments(classInstance)
      val cs_block_id = getUniqueId.toString
      <tr>
        <th>{ implicitly[Show[Person]].toHTML(person) }</th>
        <td>{ implicitly[Show[Subject]].toHTML(subject) }</td>
        <td>{ show_question_stats(quality) }</td>
        <td>{
          val s = quality.xs.find(_.of.question == understandingQuestion).get
          show_mean(s) ++ dumpForSparkbar(s, 1 to 7)
        }</td>
        <td>{ show_attendance_stats(attendance) }</td>
        <td>
          { quality.sample_size }
          <span style="font-size: 0.6em">({showPercent(samplePercent(quality))})</span>
        </td>
        <td>{ show_comments_link(cs, cs_block_id) }</td>
      </tr>
      <tr class="comments" id={ "comments-" ++ cs_block_id }>
        <td colspan="8">
          { show_comments(cs) }
        </td>
      </tr>
    }).flatten
  }

  def show_per_person_stats(xs: List[ClassStats], limitPercent: Int, comments: ClassInstance => List[(Class, String)])
    (implicit ord: Ordering[ClassStats]): NodeSeq = {
    def keep(x: ClassStats) = x.quality.sample_size >= 5
    def takeTopPercent[T](xs: List[T], p: Int)(implicit ord: Ordering[T]) = {
      val n = scala.math.ceil((p * xs.size: Double) / 100).toInt
      val (ys1, ys2) = xs.sorted splitAt n
      val last = ys1.last
      ys1 ++ (ys2 takeWhile (ord.equiv(last, _)))
    }
    <table>
      <thead>
        <tr>
          <th>Osoba</th>
          <th>Przedmiot</th>
          <th>Oceny</th>
          <th>Średnia zrozumiałość</th>
          <th>Obecność (%)</th>
          <th>Próbka</th>
          <th>Komentarze</th>
        </tr>
      </thead>
      <tbody>{
        for {
          (classType, cxs) <- xs groupBy (_.of.classType)
          (preserved, discarded) = cxs partition keep
        } yield {
          <tr class="class-type-header">
            <th colspan="7">Zajęcia typu: { classType } (odrzuconych {showPercent(percent(discarded.size, cxs.size))})</th>
          </tr> ++
          show_per_person_stats_rows(takeTopPercent(preserved, limitPercent), comments)
        }
      }</tbody>
    </table>
  }

  def showCategorized(xs: List[ClassStats], title: Category => String, show: List[ClassStats] => NodeSeq,
    categorization: Categorization): NodeSeq = {
    implicit val ordering = categorization.ordering
    val categorized = (xs groupBy (x => categorization assign (x.of.subject))).toList sortBy (_._1)
    (for ((category, cxs) <- categorized) yield {
       <h3>{title(category)}</h3> ++ show(cxs)
    }).flatten
  }

  def buildReport(answers: List[Survey], categorization: Categorization): NodeSeq = {
    val rankingPercent = 25
    val statsByQuestion = StatsGenerator.statsByQuestion(answers)
    val statsByClassType = StatsGenerator.statsByClassType(answers).sortBy(-_.quality.mean)
    val statsByTitle = StatsGenerator.statsByTitle(answers).sortBy(-_.quality.mean)
    val statsByPosition = StatsGenerator.statsByPosition(answers).sortBy(-_.quality.mean)
    val statsByAggregatedPosition = StatsGenerator.statsByAggregatedPosition(answers).sortBy(-_.quality.mean)
    val statsByPersonSubject = StatsGenerator.statsByPersonSubject(answers).sortBy(-_.quality.mean)
    val (quality, relativeFilled) = (for {
           CompleteStats(ClassInstance(person, subject, _), qualityStats, _) <- statsByPersonSubject
      val surveys = answers.filter(x => x.clazz.subject == subject && x.person == person)
      val ratios = for {
        x <- surveys
        answer <- x.values
        val qs = answer.qi.qs
      } yield (qs.filled: Double) / qs.allowed * 100
    } yield (qualityStats.mean, ratios.max)).unzip

    val statsByQuestionMatrix = StatsGenerator.statsByQuestionMatrix(answers)

    val comments: ClassInstance => List[(Class, String)] =
                (x: ClassInstance) => StatsGenerator.getCommentsForPersonSubject(answers, x.person, x.subject)

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
          <a href="http://github.com/rawicki/ankiety">
            <img style="position: absolute; top: 0; left: 0; border: 0;" src="http://s3.amazonaws.com/github/ribbons/forkme_left_green_007200.png" alt="Fork me on GitHub" />
          </a>
          <h1>Wyniki ankiet 2009Z/2009L (Publishing Report)</h1>
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
                    <th>{ implicitly[Show[Question]].toHTML(stats.of) }</th>
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
            <h2>Najlepsze wyniki (osoba, przedmiot)</h2>
            {
              implicit val ord = Ordering.by[ClassStats, Double](_.quality.mean).reverse
              showCategorized(statsByPersonSubject, _.title(rankingPercent), show_per_person_stats(_, rankingPercent,
                comments), categorization)
            }
          </div>
          <div class="center">
            <h2>{rankingPercent}% najgorszych wyników (osoba, przedmiot)</h2>
            {
              implicit val ord = Ordering.by[ClassStats, Double](_.quality.mean)
              show_per_person_stats(statsByPersonSubject, rankingPercent, comments)
            }
          </div>
          <div class="center">
            <h2>{rankingPercent}% najbardziej kontrowersyjnych wyników (osoba, przedmiot)</h2>
            {
              implicit val ord = Ordering.by[ClassStats, Double](_.quality.dev).reverse
              show_per_person_stats(statsByPersonSubject, rankingPercent, comments)
            }
          </div>
          <div class="center">
            <h2>{rankingPercent}% najczęściej opuszczanych zajęć (osoba, przedmiot)</h2>
            {
              implicit val ord = Ordering.by[ClassStats, Double](_.attendance.mean)
              show_per_person_stats(statsByPersonSubject, rankingPercent, comments)
            }
          </div>
          <div class="center">
            <h2>Ocena prowadzącego a procent wypełnionych ankiet</h2>
            { scatterPlot(quality zip relativeFilled) }
          </div>
        </body>
      </html>
    report
  }
    
}

