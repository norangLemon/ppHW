package submission
import Data._
import scala.annotation.tailrec

/*
 Implement below functions, which is currently blank. (???)
 Before asking for clarification of problem statement, look through test data.
 */

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

  def calculate(x: Exp): Int =
    x match {
      case EInt(i) => i
      case EAdd(l, r) => calculate(l) + calculate(r)
      case ESub(l, r) => calculate(l) - calculate(r)
      case EMul(l, r) => calculate(l) * calculate(r)
    }
}
