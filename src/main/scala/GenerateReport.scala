import java.io.{OutputStreamWriter, FileOutputStream}
import scala.xml._

import surveys.DataImporter.DataImporter
import surveys.ReportBuilder.ReportBuilder

object GenerateReport {
  def main(args: Array[String]){
    val salt = if (args contains "md5") Some((1 to 10).map(_ => scala.util.Random.nextPrintableChar).mkString("")) else None
    val answers = (new DataImporter(salt)).readSurveys

    val report = ReportBuilder.buildReport(answers)

    val fw = new OutputStreamWriter(new FileOutputStream("Report.html"), "UTF-8")
    fw.write(report.toString)
    fw.close()
  }
}
