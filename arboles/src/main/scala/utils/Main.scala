package utils
import utils.ArbolBinario._;

object Main extends App {
  
  println(utils.ArbolBinario.armarArbol(insert(3,
      insert(2,
        insert(1,
          EmptyTree())))));
  
  println(utils.ArbolBinario.armarArbol(utils.ArbolBinario.balancear(insert(3,
      insert(2,
        insert(1,
          EmptyTree()))))));
  
}
