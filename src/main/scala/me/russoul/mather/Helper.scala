package me.russoul.mather

object Helper {

  import Mather._

  val NegEInf = EBinFn(EInt(-1), EInf, Mult)

  def findDomain(expr : Expr) : Option[IInterval] = {
    expr match{

    }
  }


  //TODO automatic solvers of equation: f(x) = 0; (given expr `a` solver would find the type of equation and solve it if type is known)

  //ax + b = 0
  def solveLinearEq(a : Expr, b : Expr) : Expr = {
    e"-1 * $b / (2 * $a)"
  }

  //ax^2 + bx + c = 0
  def solveQuadraticEq(a : Expr, b : Expr, c : Expr) : List[Expr] = {
    val d = e"$b * $b - 4 * $a * $c"
    List(e"(-1 * $b + sqrt($d))/(2 * $a)", e"(-1 * $b - sqrt($d))/(2 * $a)")
  }

}
