import org.scalatest._
import submission.main._
import Data._

class TestSuite extends FunSuite {
  def listIntToIList(xs: List[Int]): IList =
    xs match {
      case (h :: t) => ICons(h, listIntToIList(t))
      case Nil => INil
    }
  test("map") {
    val a = listIntToIList(List(1, 2, 3, 4))
    val b = listIntToIList(List(2, 4, 6, 8))
    val c = listIntToIList(List(11, 12, 13, 14))
    assert(map(a)(_ * 2) == b)
    assert(map(a)(10 + _) == c)
  }

  test("reverse") {
    val a = listIntToIList(List(1, 2, 3, 4))
    val b = listIntToIList(List(4, 3, 2, 1))
    assert(reverse(a) == b)
  }

  test("exp") {
    val three = EInt(3)
    val two = EInt(2)
    assert(calculate(EAdd(two, three)) == 5)
    assert(calculate(ESub(two, three)) == -1)
    assert(calculate(EMul(two, three)) == 6)
    assert(calculate(EAdd(two, EAdd(two, EAdd(two, two)))) == 8)
  }
}
