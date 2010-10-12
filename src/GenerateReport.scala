import scala.io.Source
//import scala.collection.mutable.Map

object GenerateReport {

  case class Subject(period: String, code: String, description: String)

  case class Class(subject: Subject, id: String, code: String, description: String)

  case class Person(id: String, title: String, name: String, lastName: String, unitCode: String, unit: String)

  case class Question(id: String, order: String, value: String)

  case class Answer(question: Question, value: Int, description: String)

  case class Answers(id: String, clazz: Class, person: Person, values: List[Answer], comment: Option[String])

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

  def openDataFile(filename: String) = {
    val lines = Source.fromFile(filename).getLines

    val rows = lines.map(_.split(';'))
    val legend = rows.next()
    val answers =
        for(item <- rows)
        yield Map(legend.zip(item).toList: _*)
    answers
  }

  /**def groupBy(category: String, collection: Iterator[Map[String, String]]) = {
    def splitMapsByCommonParts(collection: Iterator[Map[String, String]]) {
      var pattern = None:Option[Map[String,String]]
      val col = collection.toList
      for(map <- col){
        pattern match {
          case None => {
            pattern = Some(map)
            println(pattern)
          }
          case Some(p) => {
            for((key, value) <- map){
              if(p.getOrElse(key, value) != value){
                p -= key
              }
            }
            println(pattern)
            println(map("jednostka"))
          }
        }
      }

      println(pattern)
      (pattern, None)
    }

    var splittedCollection = Map()
    for(map <- collection)

    splitMapsByCommonParts(collection)
  }**/

	def personMeanAndDevation(xs: List[Answers]): Map[Person, Map[Question, (Double, Double)]] = {
		def meanAndDevation(xs: List[Int]): (Double, Double) = {
			val mean = (xs.sum: Double) / xs.size
			val variance = xs.foldLeft(0: Double) {
				case (acc, x) => ((x-mean)*(x-mean): Double)/xs.size + acc
			}
			(mean, scala.math.sqrt(variance))
		}
		val perPerson: Map[Person, List[Answers]] = xs groupBy (_.person)
		perPerson mapValues { xs =>
			val questions: Map[Question, List[Answer]] = (for (x <- xs; answer <- x.values) yield answer) groupBy (_.question)
			questions mapValues (xs => meanAndDevation(xs map (_.value)))
		}
	}

  def main(args: Array[String]){
    val answers = readAnswers
		val xs: List[(Person, Question, (Double, Double))] = for {
			(person, qs) <- personMeanAndDevation(answers).toList
			(question, (mean, devation)) <- qs.toList
		} yield (person, question, (mean, devation))
		val byDevation = xs sortBy {
			case (_, _, (_, devation)) => -devation
		}
		for ((person, question, (mean, devation)) <- byDevation take 40) {
			println(person)
			println("Mean %1f, Devation %2f and Question %3s".format(mean, devation, question.toString))
		}
  }
}
