package surveys

import java.io.{OutputStreamWriter, FileOutputStream}
import scala.xml._

import surveys.model.{Survey, Subject}
import surveys.DataImporter.Data
import surveys.report._
import surveys.SurveySet._

object GenerateReport {
  def generateReport(surveySet: SurveySet, title: String, c: Categorization, prefixes: List[String]) {
    {
      print("PublishingReport")
      val report = new PublishingReport(surveySet, c, prefixes.map(_.stripPrefix("1000-").toUpperCase))
      scala.xml.XML.save(filename = title + ".html", node = report.buildReport,
        enc = "UTF-8", doctype = report.doctype)
      println(".")
    }
    {
      print("CompleteReport")
      val report = new CompleteReport(surveySet, c, prefixes.map(_.stripPrefix("1000-").toUpperCase))
      scala.xml.XML.save(filename = title + "-complete.html", node = report.buildReport,
        enc = "UTF-8", doctype = report.doctype)
      println(".")
    }
    {
      print("PersonalReport")
      val report = new PersonalReport(surveySet, c, prefixes.map(_.stripPrefix("1000-").toUpperCase))
      scala.xml.XML.save(filename = title + "-complete.html", node = report.buildReport,
        enc = "UTF-8", doctype = report.doctype)
      println(".")
    }
  }

  def main(args: Array[String]) {
    val salt = if (args contains "md5") Some((1 to 10).map(_ => scala.util.Random.nextPrintableChar).mkString("")) else None
    val prefixes = {
      val xs = args.filterNot(_=="md5").toList
      if (xs != Nil) xs else findDataPrefixes
    }
    prefixes foreach { x =>
      println("Running generate report with prefix " + x)
      val answers = (new Data(salt, x :: Nil)).readSurveys
      println("Generating complete reports:")
      generateReport(new All(answers), x + "_Report", OneCatCategorization, List(x))
      println("Generating reports for mathematics:")
      generateReport(new Math(answers), x + "_Mathematics", MathCategorization, List(x))
      println("Generating reports for computer science:")
      generateReport(new CS(answers), x + "_ComputerScience", CSCategorization, List(x))
      println("Generating reports for bioinformatics:")
      generateReport(new Bio(answers), x + "_Bioinformatics", OneCatCategorization, List(x))
      println("")
    }
  }

  def findDataPrefixes: List[String] = {
    val dataDir = new java.io.File("data/")
    if (!dataDir.exists)
      sys.error("data directory doesn't exist")
    val prefixes = for (x <- dataDir.list if x endsWith "_zajecia_wszystko.csv") yield
      x stripSuffix "_zajecia_wszystko.csv"
    prefixes.toList.distinct
  }

  def sameYearPrefixes(xs: List[String]): List[List[String]] = {
    val yearPattern = """.*1000-([0-9]+)[lz].*""".r
    (xs groupBy { x =>
      val yearPattern(year) = x
      year
    }).values.toList
  }
}
