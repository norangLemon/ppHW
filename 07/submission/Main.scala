package submission
import Data.DataBundle._

object TraitMixin extends App {
  /*
   Below add/mult operator is common sense, no special spec is specified.
   Types and test cases below may sufficient for clarification.

   For error range, your implementation should not have error bigger than 1E-7.
   If you use normal "Double" operators, you may not need to worry about it at all.
   */
  trait ComplexNumberAdd extends ComplexNumberSpec {
    def add(that: ComplexNumberSpec): ComplexNumberSpec = ???
  }

  trait ComplexNumberMult extends ComplexNumberSpec {
    def mult(that: ComplexNumberSpec): ComplexNumberSpec = ???
  }

  /*
   Spec of eval
   Input: A polynomial, represented in list of coefficients.
          For example, (1, 0, i, -1) = 1 + (0 * x) + (i * x^2) + (-1 * x^3)
   Output: Calculated result of given polynomial function with "this" as an input.
          For example, if (1 + 0*i) calls "eval" with above input, result will be
          1 + (0 * (1 + 0*i)) + (i * (1 + 0*i)^2) + (-1 * (1 + 0*i)^3)
          = 1 + 0 + i - 1
          = 0 + i
   */
  trait ComplexNumberEval
      extends ComplexNumberSpec
      with ComplexNumberAdd
      with ComplexNumberMult {
    def eval(coeffs: List[ComplexNumberEval]): ComplexNumberSpec = ???
  }

  /*
   You may get compile error for now, for missing signatures.
   Implement each missing fields so that it compiles.
   For constructor,
     if "isRectangular" is true,
       use "arg1" as "real" and "arg2" as "imaginary" in rectangular representation.
     else,
       use "arg1" as "magnitude" and "arg2" as "angle" in polar representation.
   */
  class ComplexNumberImpl (isRectangular: Boolean, arg1: Double, arg2: Double)
      extends ComplexNumberSpec
      with ComplexNumberAdd
      with ComplexNumberMult
      with ComplexNumberEval {
  }




  //Below is some simple test cases for sanity check
  val a = new ComplexNumberImpl(true, 0, 1)
  val b = new ComplexNumberImpl(true, 3, 4)
  val c = new ComplexNumberImpl(false, 1, scala.math.Pi)
  assert(doubleEqFunc(a.magnitude, 1))
  assert(doubleEqFunc(a.angle, 1.5708))
  assert(doubleEqFunc(b.magnitude, 5))
  assert(doubleEqFunc(b.angle, 0.9273))
  assert(doubleEqFunc(c.real, -1))
  assert(doubleEqFunc(c.imaginary, 0))

  assert(a.add(b).eqFunc(new ComplexNumberImpl(true, 3, 5)))
  assert(a.mult(b).eqFunc(new ComplexNumberImpl(true, -4, 3)))
  assert(a.add(c).eqFunc(new ComplexNumberImpl(true, -1, 1)))
  assert(a.mult(c).eqFunc(new ComplexNumberImpl(true, 0, -1)))

  val one = new ComplexNumberImpl(true, 1, 0)
  val zero = new ComplexNumberImpl(true, 0, 0)

  val t1 = (new ComplexNumberImpl(true, 10, 0)).eval(List(one, one, one))
  assert(t1.eqFunc(new ComplexNumberImpl(true, 111, 0)))
  //(1 + x + x^2)[x := 10] gives 111

  val t2 = (new ComplexNumberImpl(true, 0, 1)).eval(List(one, zero, one))
  assert(t2.eqFunc(new ComplexNumberImpl(true, 0, 0)))
  //(1 + x^2)[x := i] gives 0
}
