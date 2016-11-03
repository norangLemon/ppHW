package submission
import Data.DataBundle._

object MoreAbstractClass {
  /*
   Fill in the ???s
   You may find some List library's methods are useful, such as drop, take.
   You may need to create your own class in this exercise.
   getPrev should move iterator once, to the opposite direction of getNext.
   Behavior of getPrev/getNext in corner cases are clarified in Test.scala.
   */
  class BiIterableList[A](val data: List[A]) extends BiIterable[A] {
    def biIter: BiIter[A] = ???
  }

  sealed abstract class BiIterableTree[A] extends BiIterable[A] {
    //You are allowed to write something here
  }

  case class Empty[A]() extends BiIterableTree[A] {
    def biIter = ???
  }

  case class Node[A](value: A, left: BiIterableTree[A], right: BiIterableTree[A])
      extends BiIterableTree[A] {
    def biIter = ???
  }
}
