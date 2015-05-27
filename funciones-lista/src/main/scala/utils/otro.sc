package utils

object otro {
  println("Welcome to the Scala worksheet")
  
  
 
  
trait Lista[T]
case class Vacia[T]() extends Lista[T]
case class Llena[T](val primero: T, val resto: Lista[T]) extends Lista[T] {

		def  esVacio[T] = this match {
			case Vacia[T] => true
			case _=> false
		}

}

}






	
	