package surveys.DataImporter

import scala.io.Source
import surveys.SurveyClasses._

class Data(hashSalt: Option[String], filePrefixes: List[String]) {
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

  private class Reader(filenames: String*) {
    def exactFilename(filename: String): String = {
      val data = new java.io.File("data", filename)
      val examplename = "przyklad_" + filename
      val example = new java.io.File(examplename)

      if(!data.exists() && example.exists()){
        println("Warning: file: " + filename + " not found. Using example file.")
        examplename
      } else {
        println("Using file: " + filename)
        "data/" + filename
      }
    }

    val (headers, records) = {
      val (manyHeaders, manyRecords) =
        (for (filename <- filenames) yield openDataFileRaw(exactFilename(filename))).unzip
      val rawHeaders = manyHeaders.head
      assert(manyHeaders.forall(_ == rawHeaders), "Headers in read files %1s are incompatible: %2s".format(filenames, manyHeaders))
      rawHeaders.zipWithIndex.toMap -> manyRecords.flatten.toList
    }

    def extract(x: Vector[String], name: String): String = x(headers(name))
    def extractOpt(x: Vector[String], name: String): Option[String] = (x.lift)(headers(name))
  }

  private object SurveyReader extends Reader(filePrefixes.map(_+"_zajecia_wszystko.csv"): _*) {
    val questionStats = QuestionStatsReader.read
    def read(comments: Map[String, String], positions: Map[String, Position]): List[Survey] = {
      val parsed_answers: List[((String, Class, Person), Answer)] = for (x <- records) yield {
        val sheetId = extract(x, "kod kartki")
        val personId = extract(x, "id osoby")
        val classId = extract(x, "id zajęć")
        val questionId = extract(x, "id pytania")
        val question = parseQuestion(x)
        val questionInstance = parseQuestionInstance(x, question, questionStats((personId, classId, questionId)))
        val answer = parseAnswer(questionInstance, x)
        ((sheetId, parseClass(parseSubject(x), x), parsePerson(x, positions)), answer)
      }
      val aggregated_answers: Map[(String, Class, Person), List[Answer]] =
          (parsed_answers groupBy (_._1)) mapValues (_ map (_._2))
      (for (((sheetId, clazz, person), answers) <- aggregated_answers) yield {
          val (attendance, quality) = answers partition (_.qi.question.value.startsWith("Na ilu"))
          val comment = comments get sheetId
          Survey(sheetId, clazz, person, quality, attendance.headOption, comment)
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
      val title = rawTitle match {
        case "" => {
          warnOnce("parsePerson: no title for %1s %2s (id %2s)".format(name, lastName, id))
          None
        }
        case "prof. dr hab." => Some("prof.")
        case x => Some(x)
      }
      val position = positions get id match {
        case Some(Position(_, p_name, p_lastName, p_position, opt_unit)) =>
          if (name != p_name) warnOnce("parsePerson: name mismatch: \"" ++ name ++ "\" != \"" ++ p_name ++ "\" (id " ++ id ++ ")")
          if (lastName != p_lastName) warnOnce("parsePerson: lastName mismatch: \"" ++ lastName ++ "\" != \"" ++ p_lastName ++ "\" (id " ++ id ++ ")")
          p_position
        case None =>
          warnOnce("parsePerson: no position for %1s %2s (id %2s)".format(name, lastName, id))
          "(brak)"
      }
      Person(id, title, md5(name), md5(lastName), unitCode, unit, position)
    }
    def parseQuestion(x: Vector[String]): Question =
      //TODO: Find out what are semantics of id of a question and decide if they are relevant to us
      //for now we are just skipping id entirely
      Question(extract(x, "treść pytania"))
    def parseQuestionInstance(x: Vector[String], q: Question, qs: QuestionStats): QuestionInstance =
      QuestionInstance(q, qs, extract(x, "kolejność"))
    def parseAnswer(qi: QuestionInstance, x: Vector[String]): Answer =
      Answer(qi, extract(x, "wartość odpowiedzi").toInt, extract(x, "opis odpowiedzi"))
  }

  private object PositionsReader extends Reader(filePrefixes.map(_+"_stanowiska.csv"): _*) {
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

  private object CommentsReader extends Reader(filePrefixes.map(_+"_zajecia_komentarze.csv"): _*) {
    def read: Map[String, String] = {
      (for (x <- records) yield {
        println(x)
        extract(x, "kod kartki") -> md5(extract(x, "treść komentarza"), 50)
      }).toMap
    }
  }

  private object QuestionStatsReader extends Reader(filePrefixes.map(_+"_zajecia_grupuj_prowadzacy.csv"): _*) {
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
