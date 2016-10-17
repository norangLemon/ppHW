package submission
import Data.DataBundle._

import scala.annotation.tailrec
/*
 Implement below functions, which is currently blank. (???)
 Before asking for clarification of problem statement, look through test data.
 */

object main {

  /*
   Exercise 1: Stack and Queue
   Implement Stack / Queue functions.
   Queue can be implemented with two stacks, in purely functional way.
   */

  val mtStk = MyNil

  def push[A](stk: Stack[A])(a: A): Stack[A] = MyCons[A](a, stk)

  def pop[A](stk: Stack[A]): Option[(A, Stack[A])] =
    stk match {
      case MyCons(hd, tl) => Some(hd, tl)
      case MyNil => None
    }

  def pushList[A](seed: Stack[A])(as: List[A]): Stack[A] =
    as.foldLeft(seed)((stk, a) => push(stk)(a))

  /*
   Queue can be implemented using two stacks, efficiently.
   In other words, amortized cost is constant.
   Say there is stack A and stack B.
   When you enQ, push to stack A.
   When you deQ, merge two stacks by popping all elements from A, and pushing
   then into B. And then pop an element from B.
   Or you may further optimize this by merging only when needed.
   */
  val mtQ = (mtStk, mtStk)

  def enQ[A](q: Queue[A])(a: A): Queue[A] = {
    (push[A](q._1)(a), q._2)
  }

  def enQList[A](seed: Queue[A])(as: List[A]): Queue[A] =
    as.foldLeft(seed)((q, a) => enQ(q)(a))

  def deQ[A](q: Queue[A]): Option[(A, Queue[A])] = {
    @tailrec
    def move(q: Queue[A]): Queue[A] =
      q._1 match {
        case MyNil => q
        case MyCons(hd, tl) => move(tl, push(q._2)(hd))
      }
    val retQ = if (q._2 == MyNil) { move(q) } else { q }
    pop(retQ._2) match{
      case None => None
      case Some((value, stk)) => Some(value, (retQ._1, stk))
    }
  }

  /*
   Exercise 2: Binary Search Tree
   Binary Search Tree (https://en.wikipedia.org/wiki/Binary_search_tree) is
   basically a Binary Tree in sorted form.
   Implement insert/lookup for Binary Search Tree.

   Each time, cmparator will be given as an argument.
   You may assume that same cmparator have been used to build given tree.
   The result of cmparator(k1, k2) should be interpreted as following:
     negative: k1 < k2
     zero:     k1 == k2
     positive: k1 > k2

   For insert function, if given key already exists in the given tree,
   you should overwrite it.
   For lookup function, return the result in Option type, meaning
     if the key exists -> Some(vale)
     else -> None
   */
  def mtBST[K, V]: BST[K, V] = Leaf

  def insert[K, V]
    (t: BST[K, V])(keyValue: (K, V))(cmp: K => K => Int): BST[K, V] =
    t match {
      case Leaf => {
        Node(keyValue, Leaf, Leaf)
      }
      case Node(value, left, right) => {
        if (cmp(keyValue._1)(value._1) < 0) Node(value, insert(left)(keyValue)(cmp), right)
        else if (cmp(keyValue._1)(value._1) > 0) Node(value, left, insert(right)(keyValue)(cmp))
        else Node(keyValue, left, right)
      }
  }

  def insertList[K, V]
    (seed: BST[K, V])(keyValues: List[(K, V)])(cmp: K => K => Int) =
    keyValues.foldLeft(seed)((tree, keyValue) => insert(tree)(keyValue)(cmp))

  def lookup[K, V](t: BST[K, V])(key: K)(cmp: K => K => Int): Option[V] =
  t match {
    case Leaf => None
    case Node(value, left, right) => {
      if (cmp(key)(value._1) < 0) lookup(left)(key)(cmp)
      else if (cmp(key)(value._1) > 0) lookup(right)(key)(cmp)
      else Some(value._2)
    }
  }
}
