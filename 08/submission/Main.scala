package submission
import Data.DataBundle._
import scala.math._

object TypeClass extends App {

  /***********
   Real Numbers
   ***********/

  implicit def realAddProxy: AddOp[Real] = ???

  implicit def realMultProxy: MultOp[Real] = ???

  /***********
   Complex Numbers
   ***********/

  /*
   ComplexNumber class and its companion object ported from hw07.
   */
  object Complex extends ComplexBasic {
    def makeRectangular(real: Real, imaginary: Real): Complex = ???
    def makePolar(magnitude: Real, angle: Real): Complex = ???
  }

  /*
   Typeclass instantiations.
   */
  implicit def complexAddProxy: AddOp[Complex] = ???

  implicit def complexMultProxy: MultOp[Complex] = ???

  /***********
   Polynomials
   ***********/

  /*
   In actual test, we may use types other than complex numbers.
   */
  object Polynomial {
    def eval[A : MultOp : AddOp](poly: Polynomial[A])(a: A) : A = ???
  }

  implicit def polynomialAddProxy[A : MultOp : AddOp]: AddOp[Polynomial[A]] = ???

  implicit def polynomialMultProxy[A : MultOp : AddOp]: MultOp[Polynomial[A]] = ???




  //Random sanity checks ported from hw07.
  val a = Complex.makeRectangular(0, 1)
  val b = Complex.makeRectangular(3, 4)
  val c = Complex.makePolar(1, scala.math.Pi)
  assert(Real.equals(a.magnitude, 1))
  assert(Real.equals(a.angle, 1.5708))
  assert(Real.equals(b.magnitude, 5))
  assert(Real.equals(b.angle, 0.9273))
  assert(Real.equals(c.real, -1))
  assert(Real.equals(c.imaginary, 0))

  val add = (implicitly[AddOp[Complex]].op) _
  val mult = (implicitly[MultOp[Complex]].op) _
  assert(Complex.equals(add(a, b), (Complex.makeRectangular(3, 5))))
  assert(Complex.equals(mult(a, b), (Complex.makeRectangular(-4, 3))))
  assert(Complex.equals(add(a, c), (Complex.makeRectangular(-1, 1))))
  assert(Complex.equals(mult(a, c), (Complex.makeRectangular(0, -1))))

  val one = Complex.makeRectangular(1, 0)
  val zero = Complex.makeRectangular(0, 0)

  val t1 = Polynomial.eval(List(one, one, one))(Complex.makeRectangular(10, 0))
  assert(Complex.equals(t1, Complex.makeRectangular(111, 0)))
  //(1 + x + x^2)[x := 10] gives 111

  val t2 = Polynomial.eval(List(one, zero, one))(Complex.makeRectangular(0, 1))
  assert(Complex.equals(t2, Complex.makeRectangular(0, 0)))

  val poly = List(1.0, 1.0, 1.0, 1.0)
  println(implicitly[MultOp[Polynomial[Real]]].op(poly, poly))




  //Some property based test
  //let f(x) := (x - root_1)(x - root_2) ... (x - root_n)
  //then f(root_i) = 0
  import scala.util.Random
  val length = 40
  object RealTest {
    val roots =
      List.fill(length)(Random.nextDouble)
    val add = implicitly[AddOp[Real]]
    val mult = implicitly[MultOp[Real]]
    val polys =
      for(root <- roots) yield {
        List(add.inverse(root), mult.identity)
      }
    val polyMult = implicitly[MultOp[Polynomial[Real]]]
    val multed = polys.foldLeft(polyMult.identity)(polyMult.op)
    for(root <- roots) {
      val evaled = Polynomial.eval(multed)(root)
      assert(Real.equals(implicitly[AddOp[Real]].identity, evaled))
    }
  }

  object ComplexTest {
    val roots =
      List.fill(length)(
        if(Random.nextBoolean) Complex.makeRectangular(Random.nextDouble, Random.nextDouble)
        else Complex.makePolar(Random.nextDouble, Random.nextDouble * 2 *Pi))
    val add = implicitly[AddOp[Complex]]
    val mult = implicitly[MultOp[Complex]]
    val polys =
      for(root <- roots) yield {
        List(add.inverse(root), mult.identity)
      }
    val polyMult = implicitly[MultOp[Polynomial[Complex]]]
    val multed = polys.foldLeft(polyMult.identity)(polyMult.op)
    for(root <- roots) {
      val evaled = Polynomial.eval(multed)(root)
      assert(Complex.equals(implicitly[AddOp[Complex]].identity, evaled))
    }
  }

  object PolynomialEvalExample {
    val realPoly: List[Real] = List(1, 1, 1, 1)
    val realPolyPoly: List[Polynomial[Real]] = List.fill(4)(realPoly)
    //Evaluating realPoly * realPoly^0 + realPoly * realPoly^1 + realPoly * realPoly^2 + realPoly * realPoly^3
    println(Polynomial.eval(realPolyPoly)(realPoly))
  }

  RealTest
  ComplexTest
  PolynomialEvalExample
}
