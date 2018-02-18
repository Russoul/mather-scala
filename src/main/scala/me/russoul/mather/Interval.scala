package me.russoul.mather

import Mather._
import Helper._

// -------------------------[--------------------)---------------------------->
//                          a   <=    x     <=   b
//a and b must be constant expressions, `gt` - if `a` > `b`, if so Interval is empty

sealed trait IInterval

case class Interval(val a : Expr, val b : Expr, gt : Bool, includeA : Bool, includeB : Bool) extends IInterval {
  assume(isConst(a) && isConst(b))

  def isEmpty : Bool = {
    gt
  }

  def isPoint(simplify : Expr => Expr) : Boolean = !isEmpty && includeA && includeB && simplify(a) == simplify(b)

}

case class UnionInterval(intervals : List[IInterval]) extends IInterval
case class IntersectionInterval(intervals : List[IInterval]) extends IInterval

object Interval{
  val IR = Interval(NegEInf, EInf, false, false, false)
  val IPos = Interval(EInt(0), EInf, false, false, false)
  val INeg = Interval(NegEInf, EInt(0), false, false, false)
  val IZero = Interval(EInt(0), EInt(0), false, true, true)
  val INonZero = UnionInterval(List(INeg, IPos))
}