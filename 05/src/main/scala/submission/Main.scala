package submission
import Data.DataBundle._

object AbstractClass {
  object IterDictImpl {
    //Write empty IterDict
    def empty[K, V](eqFunc: K => K => Boolean): IterDict[K, V] = {
      val ret = new IterDict[K, V]{
        def getValue = None
        def getNext = this
        def add(k: K, v: V): IterDict[K, V] = new IterDictImpl[K, V](eqFunc)(List((k, v)))
        def find(k: K) : Option[V] = None
      }
      ret
    }
  }

  class IterDictImpl[K, V](eqFunc: K => K => Boolean)(val data: List[(K, V)])
      extends IterDict[K, V] {

    def getValue =
      data match {
        case Nil => None
        case (k, v):: _ => Some((k, v))
      }

    def getNext =
      data match {
        case Nil => this
        case (k, v) :: list=> new IterDictImpl(eqFunc)(list)
      }

    //This function should add given key/value to this dictionary.
    //As usual, when the given key already exists in this dictionary, add
    //function should overwrite that key. By overwrite, I mean the original
    //key should not affect both "find" function and "sumElements" function.
    def add(k: K, v: V) : IterDict[K, V] ={
      def reset(k: K, v: V, data:List[(K, V)]): List[(K, V)] = {
        data match {
          case (k1, v1) :: list if eqFunc(k1)(k) => (k, v) :: list
          case (k1, v1) :: list => (k1, v1) :: reset(k, v, list)
        }
      }
      this.find(k) match {
        case None =>
          data match {
            case Nil => new IterDictImpl(eqFunc)(List((k, v)))
            case _ => new IterDictImpl(eqFunc)((k, v)::data)
          }
        case Some(_) => new IterDictImpl(eqFunc)(reset(k, v, data))
      }
    }

    //Find given key in this dictionary and return its value in Option type.
    //When there is no such key, return None.
    def find(k: K) : Option[V] =
    data match {
      case (key, v) :: list => if (eqFunc(key)(k)) Some(v) else (new IterDictImpl(eqFunc)(list)).find(k)
      case Nil => None
    }
  }

  //Write a function that iterates through given iterator and sum it.
  //This is similar to what we have learned in slide, but the difference is
  //you shuold only add its Int values, using normal "+" operator.
  def sumElements[K](xs: Iter[(K, Int)]): Int =
  xs.getValue match {
    case None => 0
    case Some((a, n)) => n + sumElements(xs.getNext)
  }
}
