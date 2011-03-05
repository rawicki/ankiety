package surveys.ReportBuilder

import scala.xml._

import surveys.SurveyClasses._
import surveys.StatsGenerator.{Stats, CompleteStats, CompositeStats, ClassInstance, StatsGenerator}
import surveys.SubjectCategories.{Category, Categorization}

abstract class Report {
  type ClassStats = CompleteStats[ClassInstance, QuestionInstance]

  var next_tag_id: Int = 1

  def show_mean[T: Show](s: Stats[T]): NodeSeq =
      <span style="white-space: nowrap">
        <span title={implicitly[Show[T]].toHTML(s.of)}>{show_double(s.mean)}</span>
        <span style="font-size: 0.7em" title="Odchylenie standardowe">(&sigma;: {show_double(s.dev)})</span>
      </span>

  def show_double(d: Double): String =
      "%2.3f" format d

  def showPercent(d: Double): String = "%2.0f%%" format d

  def dumpForSparkbar(s: Stats[_], domain: Seq[Int]): NodeSeq =
      <span class="inlinesparkbar">{
          val grouped = s.xs.groupBy(identity) mapValues (_.size)
          (for (x <- domain) yield grouped.getOrElse(x, 0)).mkString(",")
      }</span>

  def show_question_stats[T: Show](s: CompositeStats[T]): NodeSeq =
      show_mean(s.flat) ++ dumpForSparkbar(s.flat, 1 to 7)

  def show_attendance_stats[T: Show](s: Stats[T]): NodeSeq =
      show_mean(s) ++ dumpForSparkbar(s, 5 to 95 by 10)

  def show_comments(comments: List[(Class, String)]): NodeSeq =
    Seq(<div class="comments">{
      for((clazz, comment) <- comments) yield Seq(
        <div class="comment"><div class="comment-header">{clazz.code}</div>{ comment }</div>
      )
    }</div>)

  def show_comments_link(comments: List[(Class, String)], id: String): NodeSeq =
    if(comments.nonEmpty){
        <a href="#" onClick={ "$(\"#comments-" ++ id ++ "\").toggle(100); return(false);" }>
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

  def samplePercent(quality: CompositeStats[QuestionInstance]) =
    (quality.sample_size: Double) / quality.xs.map(_.of.qs.allowed).max * 100

  def percent(n: Int, m: Int): Double = (n: Double) / m * 100

  def buildReport(answers: List[Survey], categorization: Categorization): NodeSeq
}

