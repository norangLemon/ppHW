import org.scalatest._
import submission.Subtype_new._

class TestSuite extends FunSuite {
  test("very simple test") {
    /*
     I intentionally gave you test with degenerated version, in order not to give
     you any hint on type signature.
     However, actual test will be done with multiple types &&
     with strct1 and strct2 having different apply functions.
     */
    val test1: Unit = {
      val mc = new myClass {
        type A = Int
        type B = Int
        type C = Int
        type D = Int
        type E = Int
        type F = Int
        type G = Int
        type H = Int
      }

      val strct1: mc.Ty1 = new {
        import mc._
        def apply: { val func: Func1 ; val c: C } => { val b: B ; val d: D } =
          x => new { val b = 0 ; val d = 0}
        val g: G = 0
      }

      val strct2: mc.Ty2 = new {
        import mc._
        def apply: { val func: Func2 ; val e: E } => { val b: B ; val f: F } =
          x => new { val b = 0 ; val f = 0}
        val h: H = 0
      }

      mc(strct1, 0, 0, 0, 0, 0, 0, 0, 0)
      mc(strct2, 0, 0, 0, 0, 0, 0, 0, 0)
    }
  }
}
