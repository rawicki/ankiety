package surveys.DataImporter

import scala.io.Source
import surveys.SurveyClasses._

object DataImporter {
  def openDataFileRaw(filename: String): List[List[String]] = {
    val lines = Source.fromFile(filename, "UTF-8").getLines
    lines.toList.map(_.split(';').toList)
  }

  def readSurveys(hashSeed: Option[Int]): List[Survey] = {
		val rawSurvey = openDataFileRaw("ankiety.csv")
		val rawComments = openDataFileRaw("komentarze.csv")
		def md5(x: String, limit: Int = 5): String = hashSeed match {
			case Some(seed) => {
				val md5 = java.security.MessageDigest.getInstance("MD5");
				md5.update(seed.toString.getBytes())
				md5.update(x.getBytes())
				md5.digest().map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _} take limit
			}
			case None => x
		}
		def parseSubject(x: List[String]): Subject = {
			val period :: code :: description :: Nil = x
			Subject(period, code, md5(description))
		}
		def parseClass(subject: Subject, x: List[String]): Class = {
			val id :: code :: description :: Nil = x
			Class(subject, id, code, description)
		}
		def parsePerson(x: List[String]): Person = {
			val id :: title :: name :: lastName :: unitCode :: unit :: Nil = x
			Person(id, if (title == "") "(unknown)" else title, md5(name), md5(lastName), unitCode, unit)
		}
		def parseQuestion(x: List[String]): Question = {
			val id :: order :: value :: Nil = x
			Question(id, order, value)
		}
		def parseAnswer(question: Question, x: List[String]): Answer = {
			val value :: description :: Nil = x
			Answer(question, value.toInt, description)
		}
		val questions: Map[String, Question] = (for (x <- rawSurvey) yield {
			val (rawQuestion, _) = (x drop 12) splitAt 3
			val q = parseQuestion(rawQuestion)
			q.id -> q
		}).toMap
		val subjects: Map[String, Subject] = (for (x <- rawSurvey) yield {
			val subject = parseSubject(x take 3)
			subject.code -> subject
		}).toMap
		val classes: Map[String, Class] = (for (x <- rawSurvey) yield {
			val (_ :: code :: _ :: Nil, rest1) = x splitAt 3
			val (rawClass, _) = rest1 splitAt 3
			val clazz = parseClass(subjects(code), rawClass)
			clazz.id -> clazz
		}).toMap
		val comments: Map[String, String] = (for (x <- rawComments) yield {
			val value :: id :: Nil = x drop 12
			id -> md5(value, 50)
		}).toMap
		val parsed_answers: List[((String, Class, Person), Answer)] = for (x <- rawSurvey) yield {
            val (rawSubject, rest1) = x splitAt 3
			val (rawClass, rest2) = rest1 splitAt 3
			val (rawPerson, rest3) = rest2 splitAt 6
			val (rawQuestion, rest4) = rest3 splitAt 3
			val (rawAnswer, sheetId :: Nil) = rest4 splitAt 2
            val question = parseQuestion(rawQuestion)
            val answer = parseAnswer(question, rawAnswer)
			((sheetId, parseClass(parseSubject(rawSubject), rawClass), parsePerson(rawPerson)), answer)
		}
    val aggregated_answers: Map[(String, Class, Person), List[Answer]] =
        (parsed_answers groupBy (_._1)) mapValues (_ map (_._2))
    val surveys: List[Survey] = (for (((sheetId, clazz, person), answers) <- aggregated_answers) yield {
        val comment = comments get sheetId
        Survey(sheetId, clazz, person, answers, comment)
    }).toList
		surveys
  }
}
