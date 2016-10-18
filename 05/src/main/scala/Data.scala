package Data

object DataBundle {
  abstract class Iter[A] {
    def getValue: Option[A]
    def getNext: Iter[A]
  }

  abstract class IterDict[K, V] extends Iter[(K, V)] {
    def add(k: K, v: V) : IterDict[K, V]
    def find(k: K) : Option[V]
  }
}
