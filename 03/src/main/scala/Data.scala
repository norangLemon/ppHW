package Data

object DataBundle {
  sealed abstract class MyList[+A]
  case object MyNil extends MyList[Nothing]
  case class MyCons[A](hd: A, tl: MyList[A]) extends MyList[A]

  type Stack[A] = MyList[A]

  type Queue[A] = (Stack[A], Stack[A])

  sealed abstract class BTree[+A]
  case object Leaf extends BTree[Nothing]
  case class Node[A](value: A, left: BTree[A], right: BTree[A]) extends BTree[A]
  type BST[K, V] = BTree[(K, V)]
}
