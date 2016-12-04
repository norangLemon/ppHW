package Data
import scala.annotation.tailrec
import scala.math._

object DataBundle {
  /*
   Polynomial type represents polynomial with coefficients in type A.
   i'th element represents coefficient of x^i term.
   If i'th element does not exist, (i bigger than list's length) it means x^i term does not exist.
   */
  type Polynomial[A] = List[A]

  type Real = Double

  object Real {
    def equals(x: Double, y: Double) = (x-y).abs <= 1E-3
  }

  /*
   You should not call this constructor directly.
   You should implement and use makeRectangular, makePolar in Complex object.
   */
  class Complex (val real: Real, val imaginary: Real, val magnitude: Real, val angle: Real)

  object ComplexPrivate {
    def rectangularToPolar(real: Double, imaginary: Double) = {
      //method arg name should be clear, and in actual code I use x/y
      val (x, y) = (real, imaginary)
      val magnitude = sqrt(pow(x, 2) + pow(y, 2))
      val angle: Double = {
        //from wikipedia
        if(x > 0) atan(y/x)
        else if(x < 0 && y >= 0) atan(y/x) + Pi
        else if(x < 0 && y < 0) atan(y/x) - Pi
        else if(x == 0 && y > 0) Pi/2
        else if(x == 0 && y < 0) -Pi/2
        else 0 //(x == 0 && y == 0), Indeterminate value.
               //This error need not be propagated above, so I didn't use option type or exception
      }
      (magnitude, angle)
    }

    def polarToRectangular(magnitude: Double, angle: Double) = {
      (magnitude * cos(angle), magnitude * sin(angle))
    }

    @tailrec
    def normalizeAngle(x: Double): Double =
      if(x > Pi) normalizeAngle(x - 2*Pi)
      else if(x <= -Pi) normalizeAngle(x + 2*Pi)
      else x
  }

  trait ComplexBasic {
    def toString(a: Complex) = {
      def formatDouble(x: Double): String =
        if(x < 0) "%.4f".format(x)
        else "+%.4f".format(x)
      s"Rectangular Form: (${formatDouble(a.real)}, ${formatDouble(a.imaginary)}), " +
      s"Polar Form: (${formatDouble(a.magnitude)}, ${formatDouble(a.angle)})"
    }

    def equals(a: Complex, b: Complex) = {
      Real.equals(a.real, b.real) &&
      Real.equals(a.imaginary, b.imaginary) &&
      Real.equals(a.magnitude, b.magnitude) &&
      (Real.equals(a.magnitude, 0) || Real.equals(a.angle, b.angle))
    }
  }


  /*
   Typeclass defining add operator.
   Below is some mathematical details you may learned from high school.
   Instance of this typeclass should satisfy the following laws:
   Associativity: op(op(a, b), c) = op(a, op(b, c))
   Commutativity: op(a, b) = op(b, a)
   Identity: for all element a of type A, op(identity, a) = identity
   Inverse: for all element a of type A, op(a, inverse(a)) = identity
   */
  abstract class AddOp[A] {
    def op(a: A, b: A): A
    val identity: A
    def inverse(a: A): A
  }

  /*
   Typeclass defining multiply operator.
   Below is some mathematical details you may learned from high school.
   Instance of this typeclass should satisfy the following laws:
   Associativity: op(op(a, b), c) = op(a, op(b, c))
   Commutativity: op(a, b) = op(b, a)
   Identity: for all element a of type A, op(identity, a) = identity
   */
  abstract class MultOp[A] {
    def op(a: A, b: A): A
    val identity: A
  }
}
