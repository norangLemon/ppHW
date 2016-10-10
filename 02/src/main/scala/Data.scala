package Data

sealed abstract class IList
case object INil extends IList
case class ICons(hd: Int, tl: IList) extends IList

sealed abstract class Exp
case class EInt(i: Int) extends Exp
case class EAdd(lhs: Exp, rhs: Exp) extends Exp
case class ESub(lhs: Exp, rhs: Exp) extends Exp
case class EMul(lhs: Exp, rhs: Exp) extends Exp
