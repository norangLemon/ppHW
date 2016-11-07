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

  class MyList[A](val data: List[A])(val i: Int) extends BiIter[A] {
    val idx = i
    def getValue: Option[A] = {
      if (data.length == i) None
      else Some(data(i))
    }
    def getNext: MyList[A] = if (i == data.length) this else new MyList[A](data)(i + 1)
    def getPrev: MyList[A] = if (i == 0) this else new MyList[A](data)(i - 1)
  }

  class BiIterableList[A](val data: List[A]) extends BiIterable[A] {
    def biIter: BiIter[A] = new MyList[A](data)(0)
  }
/*
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
  */
}
