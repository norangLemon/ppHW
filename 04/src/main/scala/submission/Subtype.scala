import scala.language.reflectiveCalls

abstract class myClass {
  type A
  type B
  type C
  type D
  type E
  type F
  type G
  type H

  type pair[X, Y] = {
    val fst: X
    val snd: Y
  }

  type ty1 = {
    def apply: { val a: A ; val c: C } => pair[B, D]
    val g: G
  }

  type ty2 = {
    def apply: { val a: A ; val e: E } => pair[B, F]
    val h: H
  }

  /*
   Find suitable common supertype of ty1 and ty2,
   and replace "Any" with that type.
   */
  type commonTy = {
    def apply: { val a: A ; val c : C ; val e: E} => {val fst: B}
  }

  /*
   Fill in the apply function here.
   It should *USE* x.
   As a consequence, the output of this function might change with different x given.
   */
  def apply(x: commonTy, _a: A, _b: B, _c: C, _d: D, _e: E, _f: F, _g: G, _h: H) =
  x.apply(new {val a = _a;val c = _c;val e = _e})
}

object subtype extends App {
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

    val strct1: mc.ty1 = new {
      import mc._
      def apply: { val a: A ; val c: C } => pair[B, D] =
        x => new { val fst = 0 ; val snd = 0}
      val g: G = 0
    }

    val strct2: mc.ty2 = new {
      import mc._
      def apply: { val a: A ; val e: E } => pair[B, F] =
        x => new { val fst = 0 ; val snd = 0}
      val h: H = 0
    }

    assert(mc(strct1, 0, 0, 0, 0, 0, 0, 0, 0).fst == 0)
    assert(mc(strct2, 0, 0, 0, 0, 0, 0, 0, 0).fst == 0)
  }
}
