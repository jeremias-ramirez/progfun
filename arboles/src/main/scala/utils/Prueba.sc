package utils

object Prueba {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def and(a:Boolean,b:Boolean) : Boolean=
  {
  		if(a) b
  		else b
  }                                               //> and: (a: Boolean, b: Boolean)Boolean
  
//val x es una funcion que te retorna un entero
 val x=  {print("-- x -- ");1 }                   //> -- x -- x  : Int = 1

// te retorna el entero si asignar pero no usa el print
 lazy val z ={print("--- z --"); 2 }              //> z: => Int
//en esta caso si usa el print y te retorna el entero asiagnado
 x+z                                              //> --- z --res0: Int = 3
 
 //aqui y a no se usa más el print
 z+z                                              //> res1: Int = 4
 
 def loop (a:Int):Int=loop(a)                     //> loop: (a: Int)Int
 
 def m (a:Int,b: =>Int) : Int =a*b                //> m: (a: Int, b: => Int)Int/
 
 m(2,loop(1))
 
}