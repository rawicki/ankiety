package surveys.ReportBuilder

import scala.xml._

import surveys.SurveyClasses._
import surveys.StatsGenerator.{Stats, CompleteStats, CompositeStats, ClassInstance, StatsGenerator}
import surveys.SubjectCategories.{Category, Categorization}

abstract class Report(answers: List[Survey], categorization: Categorization) {
  type ClassStats = CompleteStats[ClassInstance, QuestionInstance]

  val reportHeader: NodeSeq =
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

  def buildReport: NodeSeq
}

