package surveys.SurveyClasses

case class Subject(period: String, code: String, description: String){
  override def toString = description
}

case class Class(subject: Subject, id: String, code: String, description: String)

case class Position(id: String, name: String, lastName: String, position: String, unit: Option[String])

case class Person(id: String, title: String, name: String, lastName: String, unitCode: String, unit: String, position: String){
  override def toString = { title ++ " " ++ name ++ " " ++ lastName }
}

case class QuestionStats(allowed: Int, filled: Int)

case class Question(id: String, order: String, value: String, stats: QuestionStats) {
  override def toString = { value ++ " - " ++ order }
}

case class Answer(question: Question, value: Int, description: String)

case class Survey(id: String, clazz: Class, person: Person, values: List[Answer], comment: Option[String])

/**
 * Class that has columns and rows indexed by labels but can have missing value for given cell.
 **/
case class PartialMatrix[T](labels: List[String], values: Map[(String, String), T])
