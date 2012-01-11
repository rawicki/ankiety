package surveys.report

import scala.xml._

import surveys.model._
import surveys.stats._
import surveys.{Category, Categorization}
import surveys.SurveySet._

class PersonalReport(surveySet: SurveySet, categorization: Categorization, periods: List[String])
        extends Report(surveySet, categorization) {

  val displayComments = true

  val title = "Wyniki ankiet %1s - %2s".format(periods.sorted mkString "/", surveySet.name)

  def buildReport: Node = {
    val rankingPercent = 100
    val minSampleSize = 0

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
          <div id="nav">
            <nav>
              <img src="templates/star-top.png" alt="*" width="12" height="25" />
              <h2 id="navhdr">MENU</h2>
              <ul id="navmenu">
                <li><a href="#top">Góra</a></li>
                <li><a href="#questions">Pytania</a></li>
                <li><a href="#people">Z podziałem na osoby</a></li>
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
          <div class="center" id="people">
            <h2>Wyniki z podziałem na osoby</h2>
            {
              showPerPersonByQuality(statsByPersonSubject, rankingPercent, minSampleSize, comments, true)
            }
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


