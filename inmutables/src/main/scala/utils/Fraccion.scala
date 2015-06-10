package utils


/**
 * En matemáticas, una fracción, número fraccionario, (del vocablo latín frāctus, 
 * fractĭo -ōnis, roto, o quebrado)1 es la expresión de una cantidad dividida 
 * entre otra cantidad; es decir que representa un cociente no efectuado de 
 * números. Por razones históricas también se les llama fracción común, fracción 
 * vulgar o fracción decimal. El conjunto matemático que contiene a las fracciones 
 * es el conjunto de los números racionales, denotado ℚ.
 */
class Fraccion(val numerador:Int, val denominador:Int) {
  require(denominador != 0, "El denominador no puede ser 0")
  
  def this(numerador:Int) = this(numerador, 1)
  
  /**
   * En matemáticas, se define el máximo común divisor(MCD) de dos o más números 
   * enteros al mayor número entero que los divide sin dejar resto.
   * El algoritmo de Euclides, que utiliza el algoritmo de la división junto 
   * al hecho que el MCD de dos números también divide al resto obtenido de 
   * dividir el mayor entre el más pequeño.
   */
  def mcd () : Int = 
  {
    if(this.abs().numerador>denominador)
    {
      if (this.abs().numerador%denominador==0) denominador
      else new Fraccion(denominador,this.abs().numerador%denominador).mcd()
    } 
    else if (denominador%this.abs().numerador==0) numerador
    else (new Fraccion(this.abs().numerador,denominador%this.abs().numerador)).mcd()
    
  }
  
  def abs() : Fraccion=
    if(numerador<0) new Fraccion(numerador*(-1),denominador)
    else new Fraccion(numerador,denominador)
  
  def simplificar () : Fraccion = 
  {
    if(this.mcd()!=1)new Fraccion(numerador/this.mcd(),denominador/this.mcd())
    else new Fraccion(numerador,denominador)
      
  }
  
  /*
   * 
   *   a   +   c   =       ad + bc     (se multiplica cruzado y los productos de suman)
   *  --       --         --------
   *   b       d             bd        (se multiplican los denominadores)
   * 
   */
  def + (otro : Fraccion) : Fraccion = 
  {
    new Fraccion((otro.denominador*this.numerador+otro.numerador*this.denominador),otro.denominador*this.denominador).simplificar()
  }
  
  def - (otro : Fraccion) : Fraccion = new Fraccion((otro.denominador*this.numerador-otro.numerador*this.denominador),otro.denominador*this.denominador).simplificar()

  
  /*
   * Para multiplicar dos fracciones, el procedimiento es muy simple. Solo es necesario 
   * hacerlo horizontalmente, es decir, multiplicar ambos numeradores y luego ambos denominadores.
   */
  def * (otro : Fraccion) : Fraccion = new Fraccion(this.numerador*otro.numerador, this.denominador*otro.denominador)
  
  def / (otro : Fraccion) : Fraccion = new Fraccion(this.numerador*otro.denominador,this.denominador*otro.numerador)
  
  def == (otro : Fraccion) : Boolean = 
  {
    (otro.simplificar().numerador==this.simplificar().numerador && otro.simplificar().denominador==this.simplificar().denominador)
  }
  
  def < (otro : Fraccion) : Boolean = (numerador.toFloat/denominador.toFloat < otro.numerador.toFloat/otro.denominador.toFloat )
  
  def > (otro : Fraccion) : Boolean = (numerador.toFloat/denominador.toFloat > otro.numerador.toFloat/otro.denominador.toFloat )
  
  def unary_- : Fraccion = new Fraccion(numerador*(-1),denominador)
  
  override def toString() : String = numerador.toString()+"\n--\n"+denominador.toString()
}