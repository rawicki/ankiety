
import sbt._

class AnkietyProject(info: ProjectInfo) extends DefaultProject(info) with IdeaProject {

  val javacsv = "net.sourceforge.javacsv" % "javacsv" % "2.0"
  
}
