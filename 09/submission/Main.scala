package submission
import Data.DataBundle._
import Data._
import scala.language.implicitConversions

object TOP {
  /*
  ...
  */
}

object Main {
  /*
   Read codes inside Object OOP.
   Implement Type Oriented Programming version of that code, inside TOP.
   When you remove "import OOP._" and instead write "import TOP._" below,
   result should be same with the former one.

   However, there is one exception.
   Your "snuMemberUglyPrint" function should *not* use dynmaic dispatch;
   instead its result should be determined by static type.
   For example,
     first "snuMemberUglyPrint" should call "prettyPrint" of "SNUMember",
     and second "snuMemberUglyPrint" should call "prettyPrint" of "Professor".

   Important: Your code should *not* contain any "ext-ends". (remove hyphen here)
   It will be checked with grep.
   */

  import OOP._
  // import TOP._

  def main(args: Array[String]): Unit = {
    val gil = new Professor("Gil Hur", 0, List(Good, Good))
    val kwang = new Professor("Kwangkeun Yi", 1, List(Good))
    val song = new Student("Youngju Song", 2, List(A, B, C, D))
    val kim = new Student("Yoonseung Kim", 3, List(A, A))
    val park = new SNUMember("Chulsoo Park", 4)

    snuMemberUglyPrint(gil: SNUMember)
    snuMemberUglyPrint(gil)

    snuMemberPrettyPrintAll(List(gil, kwang, song, kim, park))
  }
}
