package surveys.model

case class Subject(period: String, code: String, description: String)

case class Class(subject: Subject, id: String, code: String, description: String)

case class Position(id: String, name: String, lastName: String, position: String, unit: Option[String])

case class Person(id: String, title: Option[String], name: String, lastName: String, unitCode: String, unit: String, position: String)

case class QuestionStats(allowed: Int, filled: Int)

case class Question(value: String)

case class QuestionInstance(question: Question, qs: QuestionStats, order: String)

case class Answer(qi: QuestionInstance, value: Int, description: String)

case class Survey(id: String, clazz: Class, person: Person, values: List[Answer], attendance: Option[Answer], comment: Option[String])

/**
 * Class that has columns and rows indexed by labels but can have missing value for given cell.
 **/
case class PartialMatrix[T](labels: List[String], values: Map[(String, String), T])

import scala.xml.{NodeSeq, Text}

trait Show[T] {
  def toHTML(x: T): NodeSeq
  def toString(x: T): String
}

object Show {
  implicit object StringShow extends Show[String] {
    def toHTML(x: String) = new Text(x)
    def toString(x: String) = x
  }

  implicit object QuestionShow extends Show[Question] {
    def toHTML(x: Question) = new Text(x.value)
    def toString(x: Question) = x.value
  }
  implicit object QuestionInstanceShow extends Show[QuestionInstance] {
    def toHTML(x: QuestionInstance) = QuestionShow.toHTML(x.question)
    def toString(x: QuestionInstance) = QuestionShow.toString(x.question)
  }
  implicit object SubjectShow extends Show[Subject] {
    def toHTML(x: Subject) = new Text(x.description)
    def toString(x: Subject) = x.description
  }
  implicit object PersonShow extends Show[Person] {
    def toHTML(x: Person) = {
      val title = x.title map (x => <span style="font-size: 0.8em;">{x} </span>) getOrElse NodeSeq.Empty
      title ++ new Text("%1s %2s".format(x.name, x.lastName))
    }
    def toString(x: Person) = {
      val title = x.title map (_ + " ") getOrElse ""
      title + "1%s %2s".format(x.name, x.lastName)
    }
  }
}
