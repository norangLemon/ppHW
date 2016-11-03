package Data

object DataBundle {
  abstract class Iter[A] {
    def getValue: Option[A]
    def getNext: Iter[A]
  }
  abstract class Iterable[A] {
    def iter: Iter[A]
  }

  abstract class BiIter[A] extends Iter[A] {
    def getNext: BiIter[A]
    def getPrev: BiIter[A]
  }
  abstract class BiIterable[A] {
    def biIter: BiIter[A]
  }
}
