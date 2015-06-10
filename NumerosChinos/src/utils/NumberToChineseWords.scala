package utils

object NumberToChineseWords {
  
val digits = Map(0 -> "〇", 1 -> "壹", 2 -> "貳", 3 -> "參", 4 -> "肆", 5 -> "伍", 6 -> "陸", 7 -> "柒", 8 -> "捌", 9 -> "玖");
val exponent = Map(1 -> "", 10 -> "拾", 100 -> "佰", 1000 -> "仟", 10000 -> "萬", 100000 -> "億", 1000000 -> "兆");

def format(unit: Int): String =
  {
    formatString(unit.toString().toList);
  }

  def formatString(numeros: List[Char]): String=
  {
    if(numeros.isEmpty) ""
    else digits((numeros.head.toInt-48))+exponent(math.pow(10, numeros.size-1).toInt)+formatString(numeros.tail)
  }
  
  def getExponent(num:Int):Int = {if(num==0)1 else 10^num }
 
}