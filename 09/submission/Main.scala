package submission
import Data.DataBundle._
import Data._
import scala.language.implicitConversions
import scala.language.higherKinds

object TOP {

  class SNUMember(val _name: String, val _ID: Int)

  abstract class SNUMemberSig[S] {
    def name(s: S): String
    def ID(s: S): Int
    def prettyPrint(s: S): String
  }

  implicit val SNUMember: SNUMemberSig[SNUMember] = new SNUMemberSig[SNUMember] {
    def name(s: SNUMember): String = s._name
    def ID(s: SNUMember): Int = s._ID
    def prettyPrint(s: SNUMember) = f"${s._name}%15s  [${s._ID}%05d]"
  }


  class Professor(val _name: String, val _ID: Int, val _lectureEvals: List[LectureEval]) {
    val parent: SNUMember = new SNUMember(_name, _ID)
  }
  implicit def ProfSNU(p: Professor): SNUMember = p.parent

  abstract class ProfessorSig[P] {
    val parent: SNUMemberSig[P]
    def lectureEvals(p: P): List[LectureEval]
    def lectureEvalsMean(p: P): Double
  }
  implicit def ProfSigSNUSig[P](implicit p: ProfessorSig[P]): SNUMemberSig[P] = p.parent

  implicit val Professor: ProfessorSig[Professor] = new ProfessorSig[Professor] {
    val parent: SNUMemberSig[Professor] = new SNUMemberSig[Professor] {
      def name(p: Professor): String = p._name
      def ID(p: Professor): Int = p._ID
      def prettyPrint(p: Professor) =
        SNUMember.prettyPrint(p.parent) + f": ${"Mean Lecture Eval"}%20s = ${lectureEvalsMean(p)}%.2f"
    }
    def lectureEvals(p: Professor): List[LectureEval] = p._lectureEvals
    def lectureEvalsMean(t: Professor) =
      lectureEvals(t).foldLeft(0.0)((s, i) => s + LectureEvalToDouble(i)) / lectureEvals(t).size
  }


  class Student(val _name: String, val _ID: Int, val _grade: List[Grade]) {
    val parent: SNUMember = new SNUMember(_name, _ID)
  }
  implicit def StuSNU(s: Student): SNUMember = s.parent

  abstract class StudentSig[S] {
    val parent: SNUMemberSig[S]
    def grades(s: S): List[Grade]
    def gradesMean(s: S): Double
  }
  implicit def StuSigSNUSig[S](implicit s: StudentSig[S]): SNUMemberSig[S] = s.parent

  implicit val Student: StudentSig[Student] = new StudentSig[Student] {
    val parent: SNUMemberSig[Student] = new SNUMemberSig[Student] {
      def name(s: Student): String = s._name
      def ID(s: Student): Int = s._ID
      def prettyPrint(s: Student) =
        SNUMember.prettyPrint(s.parent) + f": ${"Mean Grade"}%20s = ${Student.gradesMean(s)}%.2f"
    }
    def grades(s: Student): List[Grade] = s._grade
    def gradesMean(t: Student) =
      grades(t).foldLeft(0.0)((s, i) => s + GradeToDouble(i)) / grades(t).size
  }


  abstract class SNUMemberDyn {
    type SNUMemberT
    val data : SNUMemberT
    val proxy : SNUMemberSig[SNUMemberT]
  }
  implicit def toSNUMemberDyn[S: SNUMemberSig](s: S): SNUMemberDyn = new SNUMemberDyn {
    type SNUMemberT = S
    val data = s
    val proxy = implicitly[SNUMemberSig[S]]
  }


  def snuMemberUglyPrint[S](x: S)(implicit proxy: SNUMemberSig[S]) = ">>>>" + proxy.prettyPrint(x)

  def snuMemberPrettyPrintAll(xs: List[SNUMemberDyn]): String =
    xs.foldLeft("")((s, i) => s + (i.proxy).prettyPrint(i.data) + "\n")

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

  // import OOP._
  import TOP._

  def main(args: Array[String]): Unit = {
    val gil = new Professor("Gil Hur", 0, List(Good, Good))
    val kwang = new Professor("Kwangkeun Yi", 1, List(Good))
    val song = new Student("Youngju Song", 2, List(A, B, C, D))
    val kim = new Student("Yoonseung Kim", 3, List(A, A))
    val park = new SNUMember("Chulsoo Park", 4)

    println(snuMemberUglyPrint(gil: SNUMember))
    println(snuMemberUglyPrint(gil))

    println(snuMemberPrettyPrintAll(List(gil, kwang, song, kim, park)))
  }
}
