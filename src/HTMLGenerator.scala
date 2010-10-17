package surveys.HTMLGenerator

import java.io.FileWriter
import org.fusesource.scalate._

object HTMLGenerator {
    def write_report(template_bindings: Map[String, String]) {
      // see: http://scalate.fusesource.org/documentation/scalate-embedding-guide.html
      val engine = new TemplateEngine
      val output = engine.layout("templates/Report.scaml", template_bindings)
      val fw = new FileWriter("Report.html")
      fw.write(output)
      fw.close()
    }
}
