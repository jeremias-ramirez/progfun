package utils

import common._

/**
 * Libreria de funciones para Listas de enteros
 */
object ListIntUtils {
  
  /**
   * Buscar 
   * Dada una lista y una función de comparación, devuelve el valor que cumple la condición. 
   */
  def buscar(lista: List[Int], com:(Int, Int) => Boolean): Int =
  {
    if(lista.tail.isEmpty) lista.head
    else if( com(lista.head,buscar(lista.tail,com)) ) lista.head 
    else buscar(lista.tail,com)
  }
  /*
   * Busca el Maximo
   */
  def max(lista: List[Int]) : Int =
  {
    buscar(lista,(e1,e2)=>(e1>e2))
  }
  /*
   * Busca el minimo
   */
  def min(lista: List[Int]) : Int =
  {
    if(lista.isEmpty) 0
    else buscar(lista,(e1,e2)=>(e1<=e2))
  }
  
  
  /*
   * Busca la mediana
   * En el ámbito de la estadística, la mediana representa el 
   * valor de la variable de posición central en un conjunto de datos ordenados.
  */
  
  def mediana(lista: List[Int]) : Int = 
    if(lista.tail.isEmpty) lista.head
    else if(contar(lista)%2!=0) ObtenerElemento(QuickSort(lista),(contar(lista)/2)+1) 
    else (ObtenerElemento(QuickSort(lista),contar(lista)/2)+ObtenerElemento(QuickSort(lista),(contar(lista)/2)+1))/2
  
  def maximos(lista: List[Int],e: Int) : List[Int]= 
    filtrar(lista,(x)=>(x>e))
     
    
    def minimos(lista: List[Int],e: Int) : List[Int]= 
      filtrar(lista,(x)=>(x<e))
    
    def QuickSort(xs: List[Int]) : List[Int]=
      if(xs.isEmpty || xs.tail.isEmpty) xs
      else QuickSort(minimos(xs.tail,xs.head))++(xs.head::QuickSort(maximos(xs.tail,xs.head)))
  
      def ObtenerElemento(lista: List[Int], posicion: Int) :Int=
        if(posicion==1) lista.head
        else ObtenerElemento(lista.tail,posicion-1)
          
          
  /**
   * Cuenta los elementos
   */
  //def contar(lista: List[Int]) : Int = ???
  
  def contar(lista: List[Int]) : Int =
      lista.foldLeft(0)((sum,x)=> sum+1)
  
 def acc(lista: List[Int]) : Int =
      lista.foldLeft(0)((sum,x)=> sum+1);
  
      
  /**
   * Filtra los elementos de la lista xs segun la funcion p
   */
  def filtrar(xs: List[Int], p: Int => Boolean): List[Int] =
      if (xs.isEmpty) xs
      else if(p(xs.head)) xs.head::filtrar(xs.tail,p)
      else filtrar(xs.tail,p)
  /**
   * Filtra los elementos pares
   */
  def filtrarPares(xs: List[Int]): List[Int] =
    filtrar(xs,(x)=>(x%2==0))
  
  /**
   * Filtra los elementos multiplos de 3
   */
  def filtrarMultiplosDeTres(xs: List[Int]): List[Int] =
    filtrar(xs,(x)=>(x%3==0))
   
   /**
   * Acumula los elementos aplicandoles fx
   */
  def acumular(lista: List[Int])(fx: (Int) => Int): Int =
    if(lista.isEmpty) 0
    else lista.foldLeft(0)((acc,x)=>acc+fx(x))
    
    
  /**
   * Acumula todos los elementos de una lista
  */ 
  def acumularUnidad(lista: List[Int]): Int = 
   if(lista.isEmpty) 0
    else acumular(lista)((x)=>(x%10))
  
  /**
   * Acumula  el dobles de los elementos de una lista
   */
  def acumularDoble(lista: List[Int]): Int = 
     if(lista.isEmpty) 0
      else acumular(lista)((x)=>(x*2))  
  /**
   * Acumula el cuadrado de los elementos de una lista
   */
  def acumularCuadrado(lista: List[Int]): Int =
       if(lista.isEmpty) 0
      else acumular(lista)((x)=>(x*x))  
}
