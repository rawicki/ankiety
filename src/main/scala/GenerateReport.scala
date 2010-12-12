import java.io.{OutputStreamWriter, FileOutputStream}
import scala.xml._

import surveys.SurveyClasses.Survey
import surveys.DataImporter.DataImporter
import surveys.ReportBuilder.ReportBuilder

object GenerateReport {
  def generateReport(answers: List[Survey], title: String) {
    val report = ReportBuilder.buildReport(answers)

    val fw = new OutputStreamWriter(new FileOutputStream(title + ".html"), "UTF-8")
    fw.write(report.toString)
    fw.close()
  }

  def main(args: Array[String]){
    val salt = if (args contains "md5") Some((1 to 10).map(_ => scala.util.Random.nextPrintableChar).mkString("")) else None
    val answers = (new DataImporter(salt)).readSurveys


    generateReport(answers, "Report")
    generateReport(answers.filter(_.clazz.subject.code.startsWith("1000-1")), "Mathematics")
    generateReport(answers.filter(_.clazz.subject.code.startsWith("1000-2")), "ComputerScience")
  }
}
