import scala.annotation.tailrec
/*
 Implement below functions, which is currently blank. (???)
 Before asking for clarification of problem statement, look through test data.
 */

sealed abstract class IList
case object INil extends IList
case class ICons(hd: Int, tl: IList) extends IList

object main {
  /*
   Exercise 1: IList Map
   Write a map function that applies gives function to all elements of given IList.
   */
  def map(xs: IList)(f: Int => Int): IList =
  xs match {
    case INil => INil
    case ICons(hd, tl) => ICons(f(hd), map(tl)(f))
  }


  /*
   Exercise 2: IList Reverse
   Write a reverse function that reverse the order of given IList.
   */
  def reverse(xs: IList): IList = {
    @tailrec
    def _reverse(in: IList, out: IList): IList =
    in match {
      case INil => out
      case ICons(hd, tl) => _reverse(tl, ICons(hd, out))
    }
    _reverse(xs, INil)
  }

  /*
   Exercise 3: Exp Calculator
   Given Exp, calculate the result to Int.
   For each constructor Add/Sub/Mul, you may interpret them as
   normal integer operators: +, -, *.
   */
  sealed abstract class Exp
  case class EInt(i: Int) extends Exp
  case class EAdd(lhs: Exp, rhs: Exp) extends Exp
  case class ESub(lhs: Exp, rhs: Exp) extends Exp
  case class EMul(lhs: Exp, rhs: Exp) extends Exp
  def calculate(x: Exp): Int = ???
}
