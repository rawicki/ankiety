import java.io.{OutputStreamWriter, FileOutputStream}
import scala.xml._

import surveys.SurveyClasses.{Survey, Subject}
import surveys.DataImporter.Data
import surveys.ReportBuilder.{PublishingReport, CompleteReport}
import surveys.SubjectCategories.{Category, Categorization, CSCategorization,  OneCatCategorization, MathCategorization}
import surveys.SurveySet._

object GenerateReport {
  def generateReport(surveySet: SurveySet, title: String, c: Categorization, prefixes: List[String]) {
    {
      val report = new PublishingReport(surveySet, c, prefixes.map(_.stripPrefix("1000-").toUpperCase))
      scala.xml.XML.save(filename = title + ".html", node = report.buildReport,
        enc = "UTF-8", doctype = report.doctype)
    }
    {
      val report = new CompleteReport(surveySet, c, prefixes.map(_.stripPrefix("1000-").toUpperCase))
      scala.xml.XML.save(filename = title + "-complete.html", node = report.buildReport,
        enc = "UTF-8", doctype = report.doctype)
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
      generateReport(new All(answers), x + "_Report", OneCatCategorization, List(x))
      generateReport(new Math(answers), x + "_Mathematics", MathCategorization, List(x))
      generateReport(new CS(answers), x + "_ComputerScience", CSCategorization, List(x))
      generateReport(new Bio(answers), x + "_Bioinformatics", OneCatCategorization, List(x))
    }
  }

  def findDataPrefixes: List[String] = {
    val dataDir = new java.io.File("data/")
    if (!dataDir.exists)
      error("data directory doesn't exist")
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
