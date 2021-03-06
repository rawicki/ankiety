package surveys.report

import scala.xml._

import surveys.model._
import surveys.stats._
import surveys.{Category, Categorization}
import surveys.SurveySet._

class PublishingReport(surveySet: SurveySet, categorization: Categorization, periods: List[String])
        extends Report(surveySet, categorization) {

  val displayComments = false

  val title = "Wyniki ankiet %1s - %2s".format(periods.sorted mkString "/", surveySet.name)

  def buildReport: Node = {
    val rankingPercent = 25
    val minSampleSize = 5

    val report =
      <html>
        { reportHeader }
        <body>
          <!-- github ribbon -->
          <a href="http://github.com/rawicki/ankiety">
            <img style="position: absolute; top: 0; left: 0; border: 0;" src="http://s3.amazonaws.com/github/ribbons/forkme_left_green_007200.png" alt="Fork me on GitHub" />
          </a>
          <h1>{title}</h1>
          <h3>(wypełnionych ankiet: {surveySet.values.size})</h3>
          <h3>(bylo wyp.: {surveySet.values.groupBy( x => (x.clazz, x.person) ).map( k => k match { case (x, v) => (v.flatMap(_.attendance).map(_.qi.qs.filled) ++ List(0)).head }  ).sum })</h3>
          <h3>(bylo upr.: {surveySet.values.groupBy( x => (x.clazz, x.person) ).map( k => k match { case (x, v) => (v.flatMap(_.attendance).map(_.qi.qs.allowed) ++ List(0)).head }  ).sum })</h3>
          <div id="nav">
            <nav>
              <img src="templates/star-top.png" alt="*" width="12" height="25" />
              <h2 id="navhdr">MENU</h2>
              <ul id="navmenu">
                <li><a href="#top">Góra</a></li>
                <li><a href="#questions">Pytania</a></li>
                <li><a href="#correlations">Korelacje</a></li>
                <li><a href="#thebest">Najlepsi</a></li>
                <li><a href="#legend">Legenda</a></li>
              </ul>
              <img class="star" src="templates/star-bottom.png" alt="*" width="12" height="29" />
            </nav>
          </div>

          <div class="center" id="questions">
            <h2>Średni wynik wg pytania</h2>
            <table>
              <thead>
                <tr>
                  <th>Pytanie</th>
                  <th>Średnia&darr;</th>
                </tr>
              </thead>
              <tbody>
                {
                  implicit val statsOrd = Ordering.by((x: Stats[Question]) => x.mean).reverse
                  for(stats <- statsByQuestion.xs.sorted) yield
                    <tr>
                    <th>
                      { implicitly[Show[Question]].toHTML(stats.of) }
                      <span style="font-size: 0.6em">({questionIndices(stats.of)})</span>
                    </th>
                    <td>{ show_mean(stats) }</td>
                    </tr>
                }
              </tbody>
            </table>
          </div>
          <div>
            <h2>Rozkład całkowitej liczby odpowiedzi według pytania</h2>
            {
              val data = statsByQuestion.xs map { x =>
                "(%1d)".format(questionIndices(x.of)) -> x.sample_size
              }
              barsPlot(data, getUniqueId())
            }
          </div>
          <div>
            <h2>Rozkład liczby odpowiedzi dla ≤k pytań</h2>
            {
              def countAnswers(x: Survey) = x.values.size
              val howMany: Map[Int, List[Survey]] = {
                val partial = surveySet.values.groupBy(countAnswers)
                val min = partial.keys.min
                val max = partial.keys.max
                ((min to max) map (i => i -> partial.getOrElse(i, Nil))).toMap
              }
              def cumulative(xs: Map[Int, Int]): Map[Int, Int] = {
                val min = xs.keys.min
                val max = xs.keys.max
                (for {
                  i <- math.max(min, 1) to max
                  keys = (min to i).toList
                } yield i -> keys.map(xs).sum).toMap
              }
              val answersCounted: Map[Int, Int] = howMany.mapValues(_.size)
              val commentsCounted: Map[Int, Int] = howMany.mapValues(_.count(_.comment.isDefined))
              val cumulativeAnswersCounted = cumulative(answersCounted)
              val cumulativeCommentCounts = cumulative(commentsCounted)
              val d1 = cumulativeCommentCounts.toList.sortBy(_._2)
              val d2 = cumulativeAnswersCounted.toList.sortBy(_._1) map {
                case (i, v) => i -> (v - cumulativeCommentCounts(i))
              }
              val ticks = d1.map(_._1).sorted.map(i => i -> ("≤"+i))
              stackedBarsPlot(ticks, d1, d2, getUniqueId())
            }
          </div>
          <div class="center" id="correlations">
            <h2>Korelacja pomiędzy wynikami z pytań</h2>
            {
              showPartialMatrix(statsByQuestionMatrix, new Text("-"), true) {
                case (stats1, stats2) => {
                  def getColour(correlation: Double): String = {
                    (scala.math.min(400 - correlation * 400, 220)).toInt.toHexString * 3
                  }
                  val cor = stats1 correlationWith stats2
                  val n = stats1.sample_size
                  <span style={ "color: #" + getColour(cor) } title={"Próbka: " + n}>{show_double(cor)}</span>
                }
              }
            }
          </div>
          <div>
            <h2>Rozkład ocen prowadzących</h2>
            {
              val data = qualityHistogram map {
                case ((p1, p2), x) => "[%1.2f %2.2f)".format(p1, p2) -> x
              }
              barsPlot(data, getUniqueId())
            }
          </div>
          <div class="center" id="thebest">
            <h2>Najlepsze wyniki (osoba, przedmiot)</h2>
            {
              showCategorized(statsByPersonSubject, _.title(rankingPercent), showPerPersonByQuality(_, rankingPercent, minSampleSize,
                comments), categorization)
            }
          </div>
          <div class="center">
            <h2>Ocena (X) pary (osoba, przedmiot) a procent (Y) wypełnionych ankiet</h2>
            { scatterPlot(quality zip relativeFilled, 0) }
          </div>
          <div class="center">
            <h2>Procent wypełnionych ankiet z komentarzami a procent wypełnionych ankiet</h2>
            { scatterPlot(commentsFilled zip relativeFilled, 1) }
          </div>
          <div class="note" id="legend">
            <h2>Legenda</h2>
            <img src="templates/description.png" alt="Opis rankingów" />
            <p>Rankingi są sporządzane według zasady, że odrzucamy oceny, dla których próbka jest mniejsza od 5. Następnie
            oceny są sortowane po ogólnej ocenie i odcinane jest 25% najlepszych wyników. Jeśli na granicy odcięcia
            oceny są takie same, próg odcięcia jest przesuwany do pierwszej oceny niżej w rankingu.</p>
            <p>Niebieskie wykresy oznaczają rozkład udzielonych odpowiedzi na dane pytanie.</p>
            <br />
            <p>Raport przygotował program, który napisali: Grzegorz Kossakowski, Rafał Rawicki, Aleksander Jankowski.</p>
            <p>Kod źródłowy jest dostępny w serwisie <a href="http://github.com/rawicki/ankiety">github.com</a></p>
          </div>
        </body>
      </html>
    report
  }

}

