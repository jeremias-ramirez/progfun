package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   * En matemática, el triángulo de Pascal es una representación de los coeficientes 
   * binomiales ordenados en forma triangular. Es llamado así en honor al matemático 
   * francés Blaise Pascal, quien introdujo esta notación en 1654, en su Traité du 
   * triangle arithmétique. Si bien las propiedades y aplicaciones del triángulo fueron 
   * conocidas con anterioridad al tratado de Pascal por matemáticos indios, chinos o persas, 
   * fue Pascal quien desarrolló muchas de sus aplicaciones y el primero en organizar la 
   * información de manera conjunta.
   * 
   * Realizar una funcion que dada la columna y la fila nos devuelve el nro del triangulo de pascal.
   * 
   */
  
  def pascal(c: Int, r: Int): Int = 
    if (c == 0 && r == 0) 1 
    else if (c < 0 || r < 0) 0 
    else pascal(c, r - 1) + pascal(c - 1, r - 1) 

  /**
   * Exercise 2
   * Escriba una función llamada “existe” que indique si un objeto se encuentra 
   * dentro de una lista determinada
   */
  def existe(o: Int, lista: List[Int]) : Boolean = 
    if (lista.isEmpty) false
    else if (lista.head == o) true
    else existe(o, lista.tail)
  
  /**
   * Exercise 3
   * Realizar una funcion que indique si una lista tiene parentesis balanceados.
   * 
   **/
    
  def balance(chars: List[Char]): Boolean = {
    
      balanceInt(chars, 0)
  }
  
   def balanceInt(chars: List[Char], cont: Int) : Boolean =
   {
      if(chars.isEmpty) (cont==0)
      else if(cont<0) false
      else if(chars.head=='(') balanceInt(chars.tail,cont+1)
      else if(chars.head==')') balanceInt(chars.tail,cont-1)
      else balanceInt(chars.tail,cont)
   }
   

  

  /**
   * Exercise 4
   * Realizar una funcion que cuente cuantas convinaciones de monedas pueden pagar un valor determinado.
   * Por ejemplo hay 3 formas de pagar 4 pesos con monedas de 1 peso y 2 pesos
   * 4 = 1+1+1+1
   * 4 = 2+2
   * 4 = 2+1+1
   */
  def countChange(money: Int, coins: List[Int]): Int = 
  {
    if(coins.isEmpty) 0
    else if(money==0) 1
    else if( money< max(coins) )
      countChange(money,withoutMax(coins))
    else
      countChange(money-max(coins),coins)+countChange(money,withoutMax(coins))
  }
  
  def max (lista: List[Int]):Int =
  {
     if(lista.isEmpty) 0
     else if(lista.tail.isEmpty) lista.head
     else if(lista.head>max(lista.tail)) lista.head
     else max(lista.tail)
     
  } 
  

  def withoutMax(lista: List[Int]): List[Int]=
  {
    if(lista.isEmpty) List()
    else if(lista.head==max(lista)) lista.tail
    else lista.head::withoutMax(lista.tail)
  }
  
}
