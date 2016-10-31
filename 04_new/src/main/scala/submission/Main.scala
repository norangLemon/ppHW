package submission
import scala.language.reflectiveCalls

object Subtype_new {
  abstract class myClass {
    type A
    type B
    type C
    type D
    type E
    type F
    type G
    type H

    type Func1 = {val a: A} => {val b: B}
    type Func2 = {val b: B} => {val a: A}
    type Func = {} => {val a: A; val b : B}

    type Ty1 = {
      def apply: {val func: Func1; val c: C} => {val b: B; val d: D}
      val g: G
    }

    type Ty2 = {
      def apply: {val func: Func2; val e: E} => {val b: B; val f: F}
      val h: H
    }

    /*
     Find suitable common supertype of Ty1 and Ty2,
     and replace "Any" with that type.
     */
    type CommonTy = {
      def apply: {val func: Func; val c: C; val e: E} => {val b: B}
    }

    /*
     Fill in the apply function here.
     It should *USE* x.
     As a consequence, the output of this function might change with different x given.
     */
    def apply(x: CommonTy, _a: A, _b: B, _c: C, _d: D, _e: E, _f: F, _g: G, _h: H) = {
      val _func: Func = Any => new {val a = _a; val b =  _b}
      x.apply(new {val func = _func; val c = _c; val e = _e})
    }
  }
}
