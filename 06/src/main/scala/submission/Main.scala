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

  class MyNode[A](tree: BiIterableTree[A])(i: Int, len: Int) extends BiIter[A] {
    val idx = i
    def getNext: MyNode[A] = if (i == len) this else new MyNode[A](tree)(i + 1, len)
    def getPrev: MyNode[A] = if (i == 0) this else new MyNode[A](tree)(i - 1, len)

    def getValue: Option[A] = {
      def at(tree:BiIterableTree[A], i:Int): (A, Int) =
        tree match {
          case Node(value, _ , _) if(i == 0) => (value, i)
          case Node(value, Empty(), Empty()) => (value, i)
          case Node(value, Empty(), right) => at(right, i-1)
          case Node(value, left, Empty()) => at(left, i-1)
          case Node(value, left, right) => {
            val end = at(left, i-1)
            if (end._2 == 0) end
            else at(right, end._2 - 1)
          }
        }
      if (i == len) None
      else Some(at(tree, i)._1)
    }
  }

  class MyEmpty[A]() extends BiIter[A] {
    def getValue: Option[A] = None
    def getNext: MyEmpty[A] = this
    def getPrev: MyEmpty[A] = this
  }

  sealed abstract class BiIterableTree[A] extends BiIterable[A] {
    //You are allowed to write something here
    def getNumOfNode: Int = {
      this match {
        case Empty() => 0
        case Node(value, left, right) => 1 + left.getNumOfNode + right.getNumOfNode
      }
    }
  }

  case class Empty[A]() extends BiIterableTree[A] {
    def biIter = new MyEmpty[A]
  }

  case class Node[A](value: A, left: BiIterableTree[A], right: BiIterableTree[A])
      extends BiIterableTree[A] {
    def biIter = new MyNode[A](this)(0, this.getNumOfNode)
  }
}
