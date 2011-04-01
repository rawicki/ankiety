package surveys.ReportBuilder

import scala.xml._

import surveys.SurveyClasses._
import surveys.StatsGenerator.{Stats, CompleteStats, CompositeStats, ClassInstance, StatsGenerator}
import surveys.SubjectCategories.{Category, Categorization}

class PublishingReport(answers: List[Survey], categorization: Categorization, periods: List[String])
        extends Report(answers, categorization) {

  val displayComments = false

  def buildReport: NodeSeq = {
    val rankingPercent = 25

    val report =
      <html>
        { reportHeader }
        <body>
          <!-- github ribbon -->
          <a href="http://github.com/rawicki/ankiety">
            <img style="position: absolute; top: 0; left: 0; border: 0;" src="http://s3.amazonaws.com/github/ribbons/forkme_left_green_007200.png" alt="Fork me on GitHub" />
          </a>
          <h1>Wyniki ankiet {periods.sorted mkString "/"}</h1>
          <h3>(wypełnionych ankiet: {answers.size})</h3>
          <div class="center">
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
                "<b>(%1d)</b>".format(questionIndices(x.of)) -> x.sample_size
              }
              barsPlot(data, getUniqueId())
            }
          </div>
          <div>
            <h2>Rozkład liczby odpowiedzi na pytania</h2>
            {
              def indicator[T](x: Option[T]): Int = if (x.isDefined) 1 else 0
              def countAnswers(x: Survey) =
                x.values.size + indicator(x.attendance) + indicator(x.comment)
              val howMany: Map[Int, List[Survey]] = {
                val partial = answers.groupBy(countAnswers)
                val min = partial.keys.min
                val max = partial.keys.max
                ((min to max) map (i => i -> partial.getOrElse(i, Nil))).toMap
              }
              def cumulative(xs: Map[Int, Int]): Map[Int, Int] = {
                val min = xs.keys.min
                val max = xs.keys.max
                (for {
                  i <- min to max
                  keys = (min to i).toList
                } yield i -> keys.map(xs).sum).toMap
              }
              val answersCounted: Map[Int, Int] = howMany.mapValues(_.size)
              val commentsCounted: Map[Int, Int] = howMany.mapValues(_.count(_.comment.isDefined))
              val cumulativeAnswersCounted = cumulative(answersCounted).toList.sortBy(_._1)
              val cumulativeCommentCounts = cumulative(commentsCounted).toList.sortBy(_._2)
              val d1 = cumulativeCommentCounts
              val d2 = cumulativeAnswersCounted map {
                case (i, v) => i -> (v - commentsCounted(i))
              }
              val ticks = d1.map(_._1).sorted.map(i => i -> ("≤"+i))
              stackedBarsPlot(ticks, cumulativeCommentCounts, cumulativeAnswersCounted, getUniqueId())
            }
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
            <h2>Najlepsze wyniki (osoba, przedmiot)</h2>
            {
              showCategorized(statsByPersonSubject, _.title(rankingPercent), showPerPersonByQuality(_, rankingPercent,
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
          <div class="note">
            <p>Rankingi są sporządzane według zasady, że odrzucamy oceny dla których próbka jest mniejsza od 5. Następnie
            oceny są sortowane po ogólnej ocenie i odcinane jest 25% najlepszych wyników. Jeśli na granicy odcięcia
            oceny są takie same próg odcięcia jest przesuwany do pierwszej oceny niżej w rankingu.</p>
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

