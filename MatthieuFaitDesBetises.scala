import scala.io._
import java.io._

object objAkinator {

  trait ABanimal
  case class Animal(nom:String) extends ABanimal
  case class Question(q:String, oui:ABanimal, non:ABanimal) extends ABanimal


  val a = Question("Est-ce qu'il a des ailes ?",
            Question("Est-ce qu'il a des plumes ?",
                Question("Est-ce qu'il a un goitre ?",
                  Animal("Pélican"),Animal("Pigeon")),
                Question("Est-ce qu'il a des poils ?",
                  Animal("Chauve-souris"),Animal("Ptérodactyle"))),
            Question("Est-ce qu'il ronronne ?",
              Animal("Chat"),Animal("Chien")))

  def traitement(data: ABanimal, it: Iterator[String]): ABanimal = data match {
    case Animal(nom) => result(Animal(nom), it)
    case Question(q,oui,non) if poserQuestion(q, it) => Question(q,traitement(oui,it),non)
    case Question(q,oui,non) => Question(q,oui,traitement(non,it))
  }

  def poserQuestion(question: String, it: Iterator[String]): Boolean = {
    println(question)
    if (it.next() == "o")
      true
    else
      false
  }

  def result(animal: Animal, it: Iterator[String]): ABanimal = {
    println(animal.nom)
    println("Es-ce bien cela ?")
    if (it.next() != "o") {
      println("Entrez ce qui le differencie de l'animal precedent")
      var question = it.next()
      println("Ajouter le nom de l'animal que vous aviez choisis")
      var res = it.next()
      Question(question,Animal(res),animal)
      }
    else
      animal
  }
}
