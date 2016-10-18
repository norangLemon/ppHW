import org.scalatest._
import submission.AbstractClass._
import Data.DataBundle._

class TestSuite extends FunSuite {
  def stringEq: String => String => Boolean = { x => y => (x == y) }
  class IterDictTest(mt: IterDict[String, Int]) {
    val d0 = mt
    val d1 = d0.add("A", 5)
    val d2 = d1.add("B", 10)
    val d3 = d2.add("A", 20)
    test("addAndFind") {
      assert(d0.find("A") == None)
      assert(d0.find("B") == None)
      assert(d1.find("A") == Some(5))
      assert(d1.find("B") == None)
      assert(d2.find("A") == Some(5))
      assert(d2.find("B") == Some(10))
      assert(d3.find("A") == Some(20))
      assert(d3.find("B") == Some(10))
    }

    test("sumElements") {
      assert(sumElements(d0) == 0)
      assert(sumElements(d1) == 5)
      assert(sumElements(d2) == 15)
      assert(sumElements(d3) == 30)
    }
  }
  new IterDictTest(IterDictImpl.empty[String, Int](stringEq))
}
