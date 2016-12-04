package Data

object DataBundle {
  sealed abstract class LectureEval
  case object Good extends LectureEval
  case object Normal extends LectureEval
  case object Bad extends LectureEval

  sealed abstract class Grade
  case object A extends Grade
  case object B extends Grade
  case object C extends Grade
  case object D extends Grade

  def LectureEvalToDouble(x: LectureEval) =
    x match {
      case Good => 5.0
      case Normal => 3.0
      case Bad => 1.0
    }

  def GradeToDouble(x: Grade) =
    x match {
      case A => 4.3
      case B => 3.3
      case C => 2.3
      case D => 1.3
    }
}


object OOP {
  import DataBundle._

  class SNUMember(_name: String, _ID: Int) {

    def name: String = _name

    def ID: Int = _ID

    def prettyPrint() = f"${name}%15s  [${ID}%05d]"
  }

  class Professor(_name: String, _ID: Int, _lectureEvals: List[LectureEval])
      extends SNUMember(_name, _ID) {

    def lectureEvals: List[LectureEval] = _lectureEvals

    def lectureEvalsMean =
      lectureEvals.foldLeft(0.0)((s, i) => s + LectureEvalToDouble(i)) / lectureEvals.size

    override def prettyPrint() =
      super.prettyPrint() + f": ${"Mean Lectuer Eval"}%20s = ${lectureEvalsMean}%.2f"
  }

  class Student(_name: String, _ID: Int, _grade: List[Grade])
      extends SNUMember(_name, _ID) {

    def grades: List[Grade] = _grade

    def gradesMean =
      grades.foldLeft(0.0)((s, i) => s + GradeToDouble(i)) / grades.size

    override def prettyPrint() =
      super.prettyPrint() + f": ${"Mean Grade"}%20s = ${gradesMean}%.2f"
  }

  def snuMemberUglyPrint(x: SNUMember) = println(">>>>" + x.prettyPrint)

  def snuMemberPrettyPrintAll(xs: List[SNUMember]) =
    xs.foreach(x => println(x.prettyPrint))
}
