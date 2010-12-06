package surveys.DataImporter

import scala.io.Source
import surveys.SurveyClasses._

class DataImporter(hashSalt: Option[String]) {
  def openDataFileRaw(filename: String): (Vector[String], List[Vector[String]]) = {
    val reader = new com.csvreader.CsvReader(filename, ';', java.nio.charset.Charset.forName("UTF-8"))
    reader.readHeaders
    val headers = Vector(reader.getHeaders: _*)
    val data = new scala.collection.mutable.ListBuffer[Vector[String]]
    while (reader.readRecord())
      data.append(Vector(reader.getValues: _*))
    headers -> data.toList
  }

  def md5(x: String, limit: Int = 5): String = hashSalt match {
    case Some(salt) => {
      val md5 = java.security.MessageDigest.getInstance("MD5");
      md5.update(salt.getBytes())
      md5.update(x.getBytes())
      md5.digest().map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _} take limit
    }
    case None => x
  }

  private class Reader(filename: String) {
    val (headers, records) = {
      val (rawHeaders, records) = openDataFileRaw(filename)
      rawHeaders.zipWithIndex.toMap -> records
    }
    def extract(x: Vector[String], name: String): String = x(headers(name))
    def extractOpt(x: Vector[String], name: String): Option[String] = (x.lift)(headers(name))
  }

  private object SurveyReader extends Reader("przyklad_1000-2009z_zajecia_wszystko.csv") {
    val questionStats = QuestionStatsReader.read
    def read(comments: Map[String, String], positions: Map[String, Position]): List[Survey] = {
      val questions: Map[String, Question] = (for (x <- records) yield {
        val personId = extract(x, "id osoby")
        val classId = extract(x, "id zajęć")
        val questionId = extract(x, "id pytania")
        questionId -> parseQuestion(x, questionStats((personId, classId, questionId)))
      }).toMap
      val subjects: Map[String, Subject] = (for (x <- records) yield {
        val subject = parseSubject(x)
        subject.code -> subject
      }).toMap
      val classes: Map[String, Class] = (for (x <- records) yield {
        val clazz = parseClass(subjects(extract(x, "kod przedmiotu")), x)
        clazz.id -> clazz
      }).toMap
      val parsed_answers: List[((String, Class, Person), Answer)] = for (x <- records) yield {
        val sheetId = extract(x, "kod kartki")
        val personId = extract(x, "id osoby")
        val classId = extract(x, "id zajęć")
        val questionId = extract(x, "id pytania")
        val question = parseQuestion(x, questionStats((personId, classId, questionId)))
        val answer = parseAnswer(question, x)
        ((sheetId, parseClass(parseSubject(x), x), parsePerson(x, positions)), answer)
      }
      val aggregated_answers: Map[(String, Class, Person), List[Answer]] =
          (parsed_answers groupBy (_._1)) mapValues (_ map (_._2))
      (for (((sheetId, clazz, person), answers) <- aggregated_answers) yield {
          val comment = comments get sheetId
          Survey(sheetId, clazz, person, answers, comment)
      }).toList
    }

    def parseSubject(x: Vector[String]): Subject =
      Subject(extract(x, "cykl dydaktyczny"), extract(x, "kod przedmiotu"), md5(extract(x, "nazwa przedmiotu")))
    def parseClass(subject: Subject, x: Vector[String]): Class =
      Class(subject, extract(x, "id zajęć"), extract(x, "kod zajęć"), extract(x, "opis zajęć"))
    var printedWarnings: Set[String] = Set();
    def warnOnce(x: String): Unit = { if (!(printedWarnings contains x)) { println(x); printedWarnings += x } }
    def parsePerson(x: Vector[String], positions: Map[String, Position]): Person = {
      val rawTitle = extract(x, "tytul")
      val id = extract(x, "id osoby")
      val name = extract(x, "imie")
      val lastName = extract(x, "nazwisko")
      val unitCode = extract(x, "kod jednostki")
      val unit = extract(x, "jednostka")
      val title = if (rawTitle == "")
      {
        warnOnce("parsePerson: no title for \"" ++ name ++ " " ++ lastName ++ "\" (id " ++ id ++ ")")
        "(brak)"
      } else if (rawTitle == "prof. dr hab.") "prof." else rawTitle
      val position = positions get id match {
        case Some(Position(_, p_name, p_lastName, p_position, opt_unit)) =>
          if (name != p_name) warnOnce("parsePerson: name mismatch: \"" ++ name ++ "\" != \"" ++ p_name ++ "\" (id " ++ id ++ ")")
          if (lastName != p_lastName) warnOnce("parsePerson: lastName mismatch: \"" ++ lastName ++ "\" != \"" ++ p_lastName ++ "\" (id " ++ id ++ ")")
          p_position
        case None =>
          warnOnce("parsePerson: no position for \"" ++ title ++ " " ++ name ++ " " ++ lastName ++ "\" (id " ++ id ++ ")")
          "(brak)"
      }
      Person(id, title, md5(name), md5(lastName), unitCode, unit, position)
    }
    def parseQuestion(x: Vector[String], stats: QuestionStats): Question = {
      val id = extract(x, "id pytania")
      Question(id, extract(x, "kolejność"), extract(x, "treść pytania"), stats)
    }
    def parseAnswer(question: Question, x: Vector[String]): Answer =
      Answer(question, extract(x, "wartość odpowiedzi").toInt, extract(x, "opis odpowiedzi"))
  }

  private object PositionsReader extends Reader("stanowiska.csv") {
    def parsePosition(x: Vector[String]): Position = {
      Position(extract(x, "id osoby"), md5(extract(x, "imie")), md5(extract(x, "nazwisko")), extract(x, "stanowisko"), extractOpt(x, "jednostka"))
    }
    def read: Map[String, Position] = {
      (for (x <- records) yield {
        val position = parsePosition(x)
        position.id -> position
      }).toMap
    }
  }

  private object CommentsReader extends Reader("przyklad_1000-2009z_zajecia_komentarze.csv") {
    def read: Map[String, String] = {
      (for (x <- records) yield {
        extract(x, "kod kartki") -> md5(extract(x, "treść komentarza"), 50)
      }).toMap
    }
  }

  private object QuestionStatsReader extends Reader("przyklad_1000-2009z_zajecia_grupuj_prowadzacy.csv") {
    def read: Map[(String, String, String), QuestionStats] = {
      (for (x <- records) yield {
        (extract(x, "id osoby"), extract(x, "id zajęć"), extract(x, "id pytania")) -> QuestionStats(extract(x, "uprawnieni").toInt, extract(x, "odp_na_pytanie").toInt)
      }).toMap
    }
  }

  def readSurveys: List[Survey] = {
    SurveyReader.read(CommentsReader.read, PositionsReader.read)
  }
}
