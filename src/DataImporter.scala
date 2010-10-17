package surveys.DataImporter

import scala.io.Source
import surveys.SurveyClasses._

object DataImporter {
  def openDataFileRaw(filename: String): List[List[String]] = {
    val lines = Source.fromFile(filename).getLines
    lines.toList.map(_.split(';').toList)
  }

  def readAnswers: List[Answers] = {
		val rawAnswers = openDataFileRaw("ankiety.csv")
		val rawComments = openDataFileRaw("komentarze.csv")
		def parseSubject(x: List[String]): Subject = {
			val period :: code :: description :: Nil = x
			Subject(period, code, description)
		}
		def parseClass(subject: Subject, x: List[String]): Class = {
			val id :: code :: description :: Nil = x
			Class(subject, id, code, description)
		}
		def parsePerson(x: List[String]): Person = {
			val id :: title :: name :: lastName :: unitCode :: unit :: Nil = x
			Person(id, title, name, lastName, unitCode, unit)
		}
		def parseQuestion(x: List[String]): Question = {
			val id :: order :: value :: Nil = x
			Question(id, order, value)
		}
		def parseAnswer(question: Question, x: List[String]): Answer = {
			val value :: description :: Nil = x
			Answer(question, value.toInt, description)
		}
		val questions: Map[String, Question] = (for (x <- rawAnswers) yield {
			val (rawQuestion, _) = (x drop 12) splitAt 3
			val q = parseQuestion(rawQuestion)
			q.id -> q
		}).toMap
		val subjects: Map[String, Subject] = (for (x <- rawAnswers) yield {
			val subject = parseSubject(x take 3)
			subject.code -> subject
		}).toMap
		val classes: Map[String, Class] = (for (x <- rawAnswers) yield {
			val (_ :: code :: _ :: Nil, rest1) = x splitAt 3
			val (rawClass, _) = rest1 splitAt 3
			val clazz = parseClass(subjects(code), rawClass)
			clazz.id -> clazz
		}).toMap
		val partialAnswers: Map[String, List[Answer]] = (for (x <- rawAnswers) yield {
			val questionId :: Nil = (x drop 12) take 1
			val (rawAnswer, sheetId :: Nil) = (x drop 15) splitAt 2
			sheetId -> parseAnswer(questions(questionId), rawAnswer)
		}).groupBy(_._1).mapValues(_.map(_._2))

		val comments: Map[String, String] = (for (x <- rawComments) yield {
			val value :: id :: Nil = x drop 12
			id -> value
		}).toMap

		val answers: List[Answers] = for (x <- rawAnswers) yield {
	  	val (rawSubject, rest1) = x splitAt 3
			val (rawClass, rest2) = rest1 splitAt 3
			val (rawPerson, rest3) = rest2 splitAt 6
			val (rawQuestion, rest4) = rest3 splitAt 3
			val (rawAnswer, sheetId :: Nil) = rest4 splitAt 2
			val values = partialAnswers(sheetId)
			val comment = comments get sheetId
			Answers(sheetId, parseClass(parseSubject(rawSubject), rawClass), parsePerson(rawPerson), values, comment)
		}
		answers
  }
}
