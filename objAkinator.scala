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
  
  def it = Iterator("o","n","o","n")

  def jeuApprentissage(a : ABanimal, it : Iterator[String]) : ABanimal = a match{
    case Question(q, oui, non) => {
      println(q);
      var rep = it.next();
      println(rep);
      if(rep == "o"){
	jeuApprentissage(oui,it)
      }else{
	jeuApprentissage(non,it)
      }
    }
    case Animal(n) => {
      println(n);
      var rep = it.next();
      println(rep);
      if(rep == "o"){
	println("gagne");
	a
      }else{
	println("perdu, quelle est la bonne reponse");
	rep = it.next();
	println("Quelle question permet de differencier "+rep+" de "+n+" ?");
	var question = it.next();
	println("Quelle est la reponse a cette question pour "+rep+" ?");
	var reponse = it.next();
	if(reponse == "n"){
	  Question(question,Animal(n),Animal(rep));
	}else{
	  Question(question,Animal(rep),Animal(n))
	}
      }
    }
  }

  def jeuSimple(a : ABanimal, it : Iterator[String]) : Boolean = a match {
    case Question(q, oui, non) => {
      println(q);
      var rep = it.next();
      println(rep);
      if(rep == "o"){
	jeuSimple(oui,it)
      }else{
	jeuSimple(non,it)
      }
    }
    
    case Animal(n) => {
      println(n);
      var rep = it.next();
      println(rep);
      if(rep == "o"){
	println("gagne");
	true
      }else{
	println("perdu");
	false
      }
    }
  }




}
//objAkinator.jeuSimple(objAkinator.a,Source.stdin.getLines)



