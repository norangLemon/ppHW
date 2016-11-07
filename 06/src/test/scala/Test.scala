import org.scalatest._
import submission.MoreAbstractClass._
import Data._

class TestSuite extends FunSuite {
  test("BiIterableList") {
    val x = List("A", "B", "C")
    val bx = new BiIterableList(x)
    assert(bx.biIter.getValue == Some("A"))
    assert(bx.biIter.getPrev.getValue == Some("A"))
    assert(bx.biIter.getPrev.getNext.getValue == Some("B"))
    assert(bx.biIter.getNext.getValue == Some("B"))
    assert(bx.biIter.getNext.getNext.getValue == Some("C"))
    assert(bx.biIter.getNext.getNext.getNext.getValue == None)
    assert(bx.biIter.getNext.getNext.getNext.getPrev.getValue == Some("C"))

    val y = Nil
    val by = new BiIterableList(y)
    assert(by.biIter.getValue == None)
    assert(by.biIter.getPrev.getValue == None)
    assert(by.biIter.getNext.getValue == None)
  }
/*
  test("BiIterableTree") {
    val x = Node[String]("A", Node("B", Empty(), Empty()), Node("C", Empty(), Empty()))
    assert(x.biIter.getValue == Some("A"))
    assert(x.biIter.getPrev.getValue == Some("A"))
    assert(x.biIter.getPrev.getNext.getValue == Some("B"))
    assert(x.biIter.getNext.getValue == Some("B"))
    assert(x.biIter.getNext.getNext.getValue == Some("C"))
    assert(x.biIter.getNext.getNext.getNext.getValue == None)
    assert(x.biIter.getNext.getNext.getNext.getPrev.getValue == Some("C"))
  }
  */
}
