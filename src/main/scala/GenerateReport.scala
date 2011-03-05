import java.io.{OutputStreamWriter, FileOutputStream}
import scala.xml._

import surveys.SurveyClasses.{Survey, Subject}
import surveys.DataImporter.Data
import surveys.ReportBuilder.{PublishingReport, CompleteReport}
import surveys.SubjectCategories.{Category, Categorization, CSCategorization,  OneCatCategorization, MathCategorization}

object GenerateReport {
  def generateReport(answers: List[Survey], title: String, c: Categorization) {
    val report = new CompleteReport(answers, c)

    val fw = new OutputStreamWriter(new FileOutputStream(title + ".html"), "UTF-8")
    fw.write(report.buildReport.toString)
    fw.close()
  }

  def main(args: Array[String]) {
    val salt = if (args contains "md5") Some((1 to 10).map(_ => scala.util.Random.nextPrintableChar).mkString("")) else None
    val prefixes = findDataPrefixes
    prefixes foreach { x =>
      println("Running generate report with prefix " + x)
      val answers = (new Data(salt, x :: Nil)).readSurveys
      generateReport(answers, x + "_Report", OneCatCategorization)
      generateReport(answers.filter(_.clazz.subject.code.startsWith("1000-1")), x + "_Mathematics", MathCategorization)
      generateReport(answers.filter(_.clazz.subject.code.startsWith("1000-2")), x + "_ComputerScience", CSCategorization)
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
