package submission
import Data.DataBundle._
import scala.math._

object TypeClass extends App {

  /***********
   Real Numbers
   ***********/


  implicit def realAddProxy: AddOp[Real] = new AddOp[Real] {
    def op(a: Real, b: Real): Real = a + b;
    val identity: Real = 0L;
    def inverse(a: Real): Real = -1 * a;
  }

  implicit def realMultProxy: MultOp[Real] = new MultOp[Real] {
    def op(a: Real, b: Real): Real = a * b;
    val identity: Real = 1L;
  }

  /***********
   Complex Numbers
   ***********/

  /*
   ComplexNumber class and its companion object ported from hw07.
   */
  object Complex extends ComplexBasic {
    def makeRectangular(real: Real, imaginary: Real): Complex = {
      val polar = ComplexPrivate.rectangularToPolar(real, imaginary);
      new Complex(real, imaginary, polar._1, polar._2);
    }
    def makePolar(magnitude: Real, angle: Real): Complex = {
      val rect = ComplexPrivate.polarToRectangular(magnitude, angle);
      new Complex(rect._1, rect._2, magnitude, angle);
    }
  }

  /*
   Typeclass instantiations.
   */
  implicit def complexAddProxy: AddOp[Complex] = new AddOp[Complex] {
    def op(a: Complex, b: Complex): Complex
      = Complex.makeRectangular(a.real+b.real, a.imaginary+b.imaginary);
    val identity: Complex = Complex.makeRectangular(0L, 0L);
    def inverse(a: Complex): Complex = Complex.makeRectangular(-1*a.real, -1*a.imaginary)
  }

  implicit def complexMultProxy: MultOp[Complex] = new MultOp[Complex] {
    override def op(a: Complex, b: Complex): Complex
      = Complex.makePolar(a.magnitude * b.magnitude, ComplexPrivate.normalizeAngle(a.angle + b.angle))
    override val identity: Complex = Complex.makeRectangular(1L, 0L)
  }

  /***********
   Polynomials
   ***********/

  /*
   In actual test, we may use types other than complex numbers.
   */
  object Polynomial {
    def eval[A : MultOp : AddOp](poly: Polynomial[A])(a: A) : A = {
      def calc[A : MultOp : AddOp](sum: A, pow: A, index: A, list: Polynomial[A]) : A
         = list match {
        case Nil => sum;
        case e :: l => {
          val newpow = implicitly[MultOp[A]].op(pow, index);
          val this_ = implicitly[MultOp[A]].op(pow, e);
          val sum_ = implicitly[AddOp[A]].op(sum, this_);
          calc(sum_, newpow, index, l);
        }
      }
      calc(implicitly[AddOp[A]].identity, implicitly[MultOp[A]].identity, a, poly);
    }
  }

  implicit def polynomialAddProxy[A : MultOp : AddOp]: AddOp[Polynomial[A]] = new AddOp[Polynomial[A]] {
    def op(a: Polynomial[A], b: Polynomial[A]): Polynomial[A] = {
      def f(x: Polynomial[A], y: Polynomial[A], sum: Polynomial[A]): Polynomial[A] =
      x match {
        case Nil => y match {
          case Nil => sum;
          case _ => sum++y;
        }
        case hx :: tx => y match {
          case Nil => sum++x;
          case hy :: ty => f(tx, ty, sum :+ implicitly[AddOp[A]].op(hx, hy));
        }
      }
      f(a, b, List[A]());
    }
    val identity: Polynomial[A] = List(implicitly[AddOp[A]].identity)
    def inverse(a: Polynomial[A]): Polynomial[A] = {
      def f(head: Polynomial[A], tail: Polynomial[A]): Polynomial[A]
      = tail match {
        case Nil => head;
        case h :: t => f(head:+implicitly[AddOp[A]].inverse(h), t);
      }
      f(List[A](), a);
    }
  }

  implicit def polynomialMultProxy[A : MultOp : AddOp]: MultOp[Polynomial[A]] = new MultOp[Polynomial[A]] {
    def op(a: Polynomial[A], b: Polynomial[A]): Polynomial[A] = {
      def mono(in: Polynomial[A], x: A, out:Polynomial[A]): Polynomial[A] =
      in match{
        case Nil => out;
        case h :: t => mono(t, x, out:+implicitly[MultOp[A]].op(h, x));
      }

      def f(x: Polynomial[A], y: Polynomial[A], out: Polynomial[A], prefix: Polynomial[A]) : Polynomial[A]
      = y match {
        case Nil => out;
        case h :: t => {
          val mono_mult = mono(prefix++x, h, List())
          f(x, t, implicitly[AddOp[Polynomial[A]]].op(out, mono_mult), prefix:+implicitly[AddOp[A]].identity)
        }
      }

      f(a, b, List(), List());
    }
    val identity: Polynomial[A] = List(implicitly[MultOp[A]].identity)
  }




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
