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
		val rawAnswers = openDataFileRaw("ankiety.csv") take 1000
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
		val partialAnswers: List[(String, Answer)] = for (x <- rawAnswers) yield {
	  	val (rawSubject, rest1) = x splitAt 3
			val (rawClass, rest2) = rest1 splitAt 3
			val (rawPerson, rest3) = rest2 splitAt 6
			val (rawQuestion, rest4) = rest3 splitAt 3
			val (rawAnswer, id :: Nil) = rest4 splitAt 2
			(id, parseAnswer(parseQuestion(rawQuestion), rawAnswer))
		}
		
		val comments: Map[String, String] = (for (x <- rawComments) yield {
			val value :: id :: Nil = x drop 12
			(id: String, value: String)
		}).toMap
		
		val answers: List[Answers] = for (x <- rawAnswers) yield {
	  	val (rawSubject, rest1) = x splitAt 3
			val (rawClass, rest2) = rest1 splitAt 3
			val (rawPerson, rest3) = rest2 splitAt 6
			val (rawQuestion, rest4) = rest3 splitAt 3
			val (rawAnswer, id :: Nil) = rest4 splitAt 2
			val values = partialAnswers collect { case (answerId, answer) if answerId == id => answer }
			val comment = comments get id
			Answers(id, parseClass(parseSubject(rawSubject), rawClass), parsePerson(rawPerson), values, comment)
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

	def personMeanAndVariance(xs: List[Answers]): Map[Person, Map[Question, (Double, Double)]] = {
		def meanAndVariance(xs: List[Int]): (Double, Double) = {
			val mean = (xs.sum: Double) / xs.size
			val variance = xs.foldLeft(0: Double) {
				case (acc, x) => ((x-mean)*(x-mean): Double)/xs.size + acc
			}
			(mean, variance)
		}
		val perPerson: Map[Person, List[Answers]] = xs groupBy (_.person)
		perPerson mapValues { xs =>
			val questions: Map[Question, List[Answer]] = (for (x <- xs; answer <- x.values) yield answer) groupBy (_.question)
			questions mapValues (xs => meanAndVariance(xs map (_.value)))
		}
	}

  def main(args: Array[String]){
    val answers = readAnswers
		// println(answers mkString "\n\n")
		println(personMeanAndVariance(answers) mkString "\n")
  }
}
