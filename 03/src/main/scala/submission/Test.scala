import main._

object Test extends App {
  {
    val l = List("A", "B", "C")
    val cba = pushList(mtStk)(l)
    val test = for(
      (c, ba) <- pop(cba);
      (b, a) <- pop(ba);
      (a, nil) <- pop(a)
    ) yield {
      assert(c == "C")
      assert(b == "B")
      assert(a == "A")
      assert(nil == mtStk)
    }
    assert(test.isDefined)
    println("Test For Stack Passed")
  }

  {
    val l = List("A", "B", "C")
    val abc = enQList(mtQ)(l)
    val test = for(
      (a, bc) <- deQ(abc);
      (b, c) <- deQ(bc);
      (c, nil) <- deQ(c)
    ) yield {
      assert(c == "C")
      assert(b == "B")
      assert(a == "A")
      assert(nil == mtQ)
    }
    assert(test.isDefined)
    println("Test For Queue Passed")
  }

  {
    val cmp = (scala.math.Ordering.Int.compare _).curried
    val l = List.fill(20)((scala.util.Random.nextInt(20), scala.util.Random.nextPrintableChar))
    val myTree = insertList(mtBST)(l)(cmp)
    for(i <- 0 to 20) {
      assert(lookup(myTree)(i)(cmp) == l.reverse.find(_._1 == i).map(_._2))
    }
    println("Test For BST Passed")
  }
}

