package me.russoul.mather

import Mather._
import me.russoul.mather.Mather.Expr

// -------------------------[--------------------)---------------------------->
//                          a   <=    x     <=   b
//a and b must be constant expressions, `gt` - if `a` > `b`
case class Interval(val a : Expr, val b : Expr, gt : Bool, includeA : Bool, includeB : Bool) {
  assume(isConst(a) && isConst(b))

  def isEmpty() : Bool = {
    gt
  }


}

object Interval{

}