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
          <div class="center" id="correlations">
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
          <div class="center" id="thebest">
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
          <div class="note" id="legend">
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

