package surveys.ReportBuilder

import scala.xml._

import surveys.SurveyClasses._
import surveys.StatsGenerator.{Stats, CompleteStats, CompositeStats, ClassInstance, StatsGenerator}
import surveys.SubjectCategories.{Category, Categorization}
import surveys.SurveySet._

trait Configuration {
  val displayComments: Boolean
}

abstract class Report(surveySet: SurveySet, categorization: Categorization) extends AnyRef with Configuration {
  type ClassStats = CompleteStats[ClassInstance, QuestionInstance]

  val title: String
  
  val doctype: dtd.DocType = dtd.DocType("html", dtd.SystemID("about:legacy-compat"), Nil)

  lazy val reportHeader: NodeSeq =
    <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
      <link rel="stylesheet" type="text/css" href="templates/style.css"/>
      <script type="text/javascript" src="templates/jquery-1.4.3.js"></script>
      <script type="text/javascript" src="templates/jquery.sparkline.js"></script>
      <!--[if IE]><script language="javascript" type="text/javascript" src="templates/flot/excanvas.min.js"></script><![endif]-->
      <script type="text/javascript" src="templates/flot/jquery.flot.js"></script>
      <script type="text/javascript" src="templates/flot/jquery.flot.stack.js"></script>

      <script type="text/javascript">
          $(function() {{
              /* Use 'html' instead of an array of values to pass options
              to a sparkline with data in the tag */
              $('.inlinesparkbar').sparkline('html', {{type: 'bar', barColor: '#0092bf'}});
          }});
      </script>
      <title>{title}</title>
    </head>

  val statsByQuestion = StatsGenerator.statsByQuestion(surveySet.values)
  val questionIndices = statsByQuestion.xs.map(_.of).zipWithIndex.toMap
  val questionOrdering = Ordering.by(questionIndices)
  val statsByClassType = StatsGenerator.statsByClassType(surveySet.values).sortBy(-_.quality.mean)
  val statsByTitle = StatsGenerator.statsByTitle(surveySet.values).sortBy(-_.quality.mean)
  val statsByPosition = StatsGenerator.statsByPosition(surveySet.values).sortBy(-_.quality.mean)
  val statsByAggregatedPosition = StatsGenerator.statsByAggregatedPosition(surveySet.values).sortBy(-_.quality.mean)
  val statsByPersonSubject = StatsGenerator.statsByPersonSubject(surveySet.values).sortBy(-_.quality.mean)
  val intervalSize = 0.5
  val qualityHistogram: List[((Double, Double), Int)] = {
    val xs = StatsGenerator.statsByPersonSubject(surveySet.values).map(_.quality.mean)
    val minQuality = 1
    val maxQuality = 7
    val ticks = Stream.from(0).map(minQuality + _*intervalSize).takeWhile(_<=maxQuality).toList
    val intervals = ticks.sliding(2).toList
    for (p1 :: p2 :: Nil <- intervals) yield (p1 -> p2) -> xs.count(x => p1 <= x && x < p2)
  }
  val (quality, relativeFilled) = (for {
         CompleteStats(_, qualityStats, _) <- statsByPersonSubject
  } yield (qualityStats.mean, samplePercent(qualityStats))).unzip

  val commentsFilled = for {
         CompleteStats(ClassInstance(person, subject, _), qualityStats, _) <- statsByPersonSubject
    val surveys = surveySet.values.filter(x => x.clazz.subject == subject && x.person == person)
    val commentsCount = surveys.filter(_.comment != None).size
    val allowedCount = (for {
        x <- surveys
        answer <- x.values
        val qs = answer.qi.qs
      } yield (qs.allowed)).max
  } yield (commentsCount: Double) / allowedCount * 100

  val statsByQuestionMatrix = StatsGenerator.statsByQuestionMatrix(surveySet.values, questionOrdering)

  val comments: ClassInstance => List[(Class, String)] =
              (x: ClassInstance) => StatsGenerator.getCommentsForPersonSubject(surveySet.values, x.person, x.subject)

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
            for ((_, index) <- m.labels.zipWithIndex) yield
              <th style="font-size: 0.6em; text-align: center; padding: 0;">{"(" + index + ")"}</th>
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

  def scatterPlot(data: List[(Double, Double)], id: Int): NodeSeq = {
    val plotId = "placeholder-%d" format id
    <div id={plotId} style="width:600px;height:300px;margin: auto;"></div>
    <script id="source" language="javascript" type="text/javascript">
      $(function () {{
        var d = {
          (data map { case (x, y) => "[%1s, %2s]".format(x.toString, y.toString) }).mkString("[", ",", "]")
        };
        $.plot($({Unparsed("\"#" + plotId + "\"")}), [
            {{
                data: d,
                points: {{show: true}},
            }}
          ], {{ yaxis: {{max: 100}} }}
        );
      }});
    </script>
  }

  def barsPlot(data: List[(String, Int)], id: Int): NodeSeq = {
    val plotId = "placeholder-%d" format id
    <div id={plotId} style="width:600px;height:300px;margin: auto;"></div>
    <script id="source" language="javascript" type="text/javascript">
      $(function () {{
        var d = {
          Unparsed((data.zipWithIndex map { case ((_, y), i) => "[%1d, %2d]".format(i, y) }).mkString("[", ",", "]"))
        };
        var t = {
          Unparsed((data.zipWithIndex map { case ((x, _), i) => "[%1d, \"%2s\"]".format(i, x) }).mkString("[", ",", "]"))
        }
        <xml:unparsed>
        var series = [{
          data: d,
          bars: {show: true, barWidth: 0.5, align: "center"}
        }]
        var options = {
          xaxis: {
            ticks: t
          }
        }
        </xml:unparsed>
        var id = {Unparsed("\"#" + plotId + "\"")}
        $.plot($(id), series, options);
      }});
    </script>
  }

  //FIXME: Move all plotting to separate module (object)
  def stackedBarsPlot(ticks: List[(Int, String)], data1: List[(Int, Int)], data2: List[(Int, Int)], id: Int): NodeSeq = {
    val plotId = "placeholder-%d" format id
    def formatData(xs: List[(Int, Int)]): NodeSeq =
      Unparsed((xs map { case (x, y) => "[%1d, %2d]".format(x, y) }).mkString("[", ",", "]"))
    <div id={plotId} style="width:600px;height:300px;margin: auto;"></div>
    <script id="source" language="javascript" type="text/javascript">
      $(function () {{
        var d1 = { formatData(data1) };
        var d2 = { formatData(data2) };
        var t = {
          Unparsed((ticks map { case (x, l) => "[%1d, \"%2s\"]".format(x, l) }).mkString("[", ",", "]"))
        }
        <xml:unparsed>
        var series = [{ data: d1, label: "Ankiety z komentarzami"} , { data: d2, label: "Wszystie ankiety"}]
        var options = {
          xaxis: {
            ticks: t
          },
          series: {
            stack: true,
            bars: {show: true, barWidth: 0.5, align: "center"}
          },
          legend: {
            position: "nw"
          }
        }
        </xml:unparsed>
        var id = {Unparsed("\"#" + plotId + "\"")}
        $.plot($(id), series, options);
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
        {
          if (displayComments) {
            val cs = comments(classInstance)
            <td>{ show_comments_link(cs, cs_block_id) }</td>
          } else NodeSeq.Empty
        }
      </tr> ++
      {
        if (displayComments) {
          val cs = comments(classInstance)
          <tr class="comments" id={ "comments-" ++ cs_block_id }>
            <td colspan="8">
              { show_comments(cs) }
            </td>
          </tr>
        } else NodeSeq.Empty
      }
    }).flatten
  }

  def showPerPersonByQuality(xs: List[ClassStats], limitPercent: Int,
                             comments: ClassInstance => List[(Class, String)]) = {
    implicit val ord = Ordering.by[ClassStats, Double](_.quality.mean).reverse
    val columnHeaders: String => NodeSeq = (x: String) => x match {
      case "Oceny" => Unparsed("Oceny&darr;")
      case x => new Text(x)
    }
    show_per_person_stats(xs, limitPercent, comments, columnHeaders)
  }

  def show_per_person_stats(xs: List[ClassStats], limitPercent: Int,
                            comments: ClassInstance => List[(Class, String)],
                            columnHeaders: String => NodeSeq = (new Text(_)))
    (implicit ord: Ordering[ClassStats]): NodeSeq = {
    def keep(x: ClassStats) = x.quality.sample_size >= 5
    def takeTopPercent[T](xs: List[T], p: Int)(implicit ord: Ordering[T]) = if (xs == Nil) Nil else {
      val n = scala.math.ceil((p * xs.size: Double) / 100).toInt
      val (ys1, ys2) = xs.sorted splitAt n
      val last = ys1.last
      ys1 ++ (ys2 takeWhile (ord.equiv(last, _)))
    }
    <table>
      <thead>
        <tr>
          {
            val columns = List("Osoba", "Przedmiot", "Oceny", "Średnia zrozumiałość", "Obecność (%)", "Próbka") ++
              (if (displayComments) List("Komentarze") else Nil)
            for (x <- columns) yield <th>{columnHeaders(x)}</th>
          }
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

  def buildReport: Node
}

