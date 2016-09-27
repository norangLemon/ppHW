import org.scalatest._
import submission.main._

class TestSuite extends FunSuite {
  test("ppascal") {
    assert(ppascal(0,0) == 1)
    assert(ppascal(2,1) == 3)
    assert(ppascal(4,2) == 17)
    assert(ppascal(5,2) == 31)
    assert(ppascal(6,6) == 1)
  }

  test("fibA") {
    assert(fibA(1) == 1)
    assert(fibA(2) == 1)
    assert(fibA(3) == 2)
    assert(fibA(4) == 3)
    assert(fibA(5) == 5)
    assert(fibA(10) == 55)
    assert(fibA(20) == 6765)
  }

  test("fibB") {
    assert(fibB(1) == 1)
    assert(fibB(2) == 1)
    assert(fibB(3) == 2)
    assert(fibB(4) == 3)
    assert(fibB(5) == 5)
    assert(fibB(10) == 55)
    assert(fibB(20) == 6765)
    assert(fibB(100) == BigInt("354224848179261915075"))
    assert(fibB(200) == BigInt("280571172992510140037611932413038677189525"))
    assert(fibB(300) == BigInt("222232244629420445529739893461909967206666939096499764990979600"))
    assert(fibB(400) == BigInt("176023680645013966468226945392411250770384383304492191886725992896575345044216019675"))
    assert(fibB(500) == BigInt("139423224561697880139724382870407283950070256587697307264108962948325571622863290691557658876222521294125"))
  }

  test("fibC") {
    assert(fibC(1) == 1)
    assert(fibC(2) == 1)
    assert(fibC(3) == 2)
    assert(fibC(4) == 3)
    assert(fibC(5) == 5)
    assert(fibC(10) == 55)
    assert(fibC(20) == 6765)
    assert(fibC(100) == BigInt("354224848179261915075"))
    assert(fibC(200) == BigInt("280571172992510140037611932413038677189525"))
    assert(fibC(300) == BigInt("222232244629420445529739893461909967206666939096499764990979600"))
    assert(fibC(400) == BigInt("176023680645013966468226945392411250770384383304492191886725992896575345044216019675"))
    assert(fibC(500) == BigInt("139423224561697880139724382870407283950070256587697307264108962948325571622863290691557658876222521294125"))
  }
}
