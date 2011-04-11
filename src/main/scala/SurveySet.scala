package surveys.SurveySet

import surveys.SubjectCategories._
import surveys.SurveyClasses.Survey

trait SurveySet {

  val name: String
  val values: List[Survey]

}

class Math(surveys: List[Survey]) extends SurveySet {

  val name = "matematyka"
  val values = surveys.filter(contains)

  private def contains(x: Survey): Boolean = x.clazz.subject.code.startsWith("1000-1")

}

class CS(surveys: List[Survey]) extends SurveySet {

  val name = "informatyka"
  val values = surveys.filter(contains)

  private def contains(x: Survey): Boolean = x.clazz.subject.code.startsWith("1000-2")

}

class All(surveys: List[Survey]) extends SurveySet {

  val name = "matematyka i informatyka"
  val values = surveys

}
