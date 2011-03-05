import java.io.{OutputStreamWriter, FileOutputStream}
import scala.xml._

import surveys.SurveyClasses.{Survey, Subject}
import surveys.DataImporter.Data
import surveys.ReportBuilder.{PublishingReport, CompleteReport}
import surveys.SubjectCategories.{Category, Categorization, CSCategorization,  OneCatCategorization}

object GenerateReport {
  def generateReport(answers: List[Survey], title: String, c: Categorization) {
    val report = CompleteReport.buildReport(answers, c)

    val fw = new OutputStreamWriter(new FileOutputStream(title + ".html"), "UTF-8")
    fw.write(report.toString)
    fw.close()
  }

  def main(args: Array[String]){
    val salt = if (args contains "md5") Some((1 to 10).map(_ => scala.util.Random.nextPrintableChar).mkString("")) else None
    val answers = (new Data(salt)).readSurveys


    generateReport(answers, "Report", OneCatCategorization)
    generateReport(answers.filter(_.clazz.subject.code.startsWith("1000-1")), "Mathematics", OneCatCategorization)
    generateReport(answers.filter(_.clazz.subject.code.startsWith("1000-2")), "ComputerScience", CSCategorization)
  }
}
