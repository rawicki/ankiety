package surveys.SurveyClasses

case class Subject(period: String, code: String, description: String){
  override def toString = description
}

case class Class(subject: Subject, id: String, code: String, description: String)

case class Person(id: String, title: String, name: String, lastName: String, unitCode: String, unit: String){
  override def toString = { title ++ " " ++ name ++ " " ++ lastName }
}

case class Question(id: String, order: String, value: String){
  override def toString = { value ++ " - " ++ order }
}

case class Answer(question: Question, value: Int, description: String)

case class Survey(id: String, clazz: Class, person: Person, values: List[Answer], comment: Option[String])
