package Data
import scala.math._
import scala.annotation.tailrec

object DataBundle {
  def doubleEqFunc(x: Double, y: Double) = (x-y).abs <= 1E-3

  /*
   "ComplexNumberSpec" describes complex number we learned in high school, like 5 + 2i.
   Complex number can be represented in multiple forms, and two of them are
     1. rectangular form, which "real" and "imaginary" fields represents for
     2. polar form. which "magnitude" and "angle" fields represents for.
   For "angle" field, it is in radian unit,
     and its value *must* be inside (-Pi, Pi] in your implementations.
   For more information, refer to: https://en.wikipedia.org/wiki/Complex_number
   */
  trait ComplexNumberSpec {
    val real: Double
    val imaginary: Double
    val magnitude: Double
    val angle: Double

    override def toString = {
      def formatDouble(x: Double): String =
        if(x < 0) "%.4f".format(x)
        else "+%.4f".format(x)
      s"Rectangular Form: (${formatDouble(real)}, ${formatDouble(imaginary)}), " +
      s"Polar Form: (${formatDouble(magnitude)}, ${formatDouble(angle)})"
    }

    def eqFunc(that: ComplexNumberSpec) = {
      doubleEqFunc(real, that.real) &&
      doubleEqFunc(imaginary, that.imaginary) &&
      doubleEqFunc(magnitude, that.magnitude) &&
      (doubleEqFunc(magnitude, 0) || doubleEqFunc(angle, that.angle))
    }

    def makeRectangular(real: Double, imaginary: Double): ComplexNumberSpec

    def makePolar(magnitude: Double, angle: Double): ComplexNumberSpec
  }

  object ComplexNumberSpec {
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
}
