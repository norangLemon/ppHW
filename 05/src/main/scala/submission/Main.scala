package submission
import Data.DataBundle._

object AbstractClass {
  object IterDictImpl {
    //Write empty IterDict
    def empty[K, V](eq: K => K => Boolean): IterDict[K, V] = ???
  }

  class IterDictImpl[K, V](eq: K => K => Boolean)(val data: List[(K, V)])
      extends IterDict[K, V] {

    def getValue = ???

    def getNext = ???

    //This function should add given key/value to this dictionary.
    //As usual, when the given key already exists in this dictionary, add
    //function should overwrite that key. By overwrite, I mean the original
    //key should not affect both "find" function and "sumElements" function.
    def add(k: K, v: V) : IterDict[K, V] = ???

    //Find given key in this dictionary and return its value in Option type.
    //When there is no such key, return None.
    def find(k: K) : Option[V] = ???
  }

  //Write a function that iterates through given iterator and sum it.
  //This is similar to what we have learned in slide, but the difference is
  //you shuold only add its Int values, using normal "+" operator.
  def sumElements[K](xs: Iter[(K, Int)]): Int = ???
}
