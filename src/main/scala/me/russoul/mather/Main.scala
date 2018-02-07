package me.russoul.mather

import cats.implicits._
import me.russoul.mather.Mather.{Expr, Matrix, Vector, combos, parse, show, simplifyUsingEquivRules2, solveLinearSystemSingular}
//import cats.syntax._

import me.russoul.mather.Mather._

object Main {


  /*def isTotallyThat(this_ : Expr, that : Expr) : Bool = {
    simplifyUsingEquivRules2(expr, rules)._1 == that
  }

  def isTotallyZero(expr : Expr) : Bool = {
    isTotallyThat(expr, EInt(0))
  }*/

 /* val expr = EBinFn(EInt(1), EUnFn(EInt(4), Sqrt), Plus)
  val expr1 = EBinFn(EVar("x"), EBinFn(EVar("y"), EVar("z"), Plus), Plus)


  val exp0 = EBinFn(EInt(10), EInt(5), Mult)
  val exp1 = EBinFn(EInt(1), exp0, Plus)
  val exp2 = EBinFn(exp1, EInt(2), Minus)
  val exp3 = EUnFn(exp2, Sqrt)
  val exp4 = EBinFn(exp3, EVar("x"), Plus)
  val exp5 = EBinFn(exp4, EVar("x"), Plus)

  println(s"exp5 = ${exp5.show}")
  println("exp5 simplified = " ++ simplifyUsingEquivRules2(exp5, combos).show)

  useAssocR(expr1).foreach(x => println(x.show))
  println(simplify(expr,  _ < 2).show)

  genEquivAssocCommutRecAll(expr1).foreach(x => println(x.show))*/

  //TODO mark debug message that they are debug messages
  //TODO add simplification rule : x * 1 == x
  //TODO make sure we do not have equivalent simplification rules, e.x: 0 * x == x * 0 == 0
  //TODO dynamic refinements are good to be used here for debug and for self explaned parameter constraints(port my scalac plugin for this to dotty ?)
  //TODO matrices (as expressions)
  //TODO domain of an Expr(== domain of a function it represents)

  //def matrixMultiply(a : Matrix, b : Matrix)(ref:R[a.m == b.n]) ==>
  //def matrixMultiply(a : Matrix, b : Matrix){
  //  assume(a.m == b.n)
  //  ...
  //}


  def main(args : Array[String]) : Unit = {
    val simplify = (e : Expr) => simplifyUsingEquivRules2(e, combos)

    val parsed1 = parse("sqrt (x * 1)")
    println("-----------------------------------------------------")
    parsed1.foreach(x => println(show(simplify(x))))

    e"abs(sqrt(x + 1) + y)" match{
      case e"abs(sqrt($x) + $y)" => println(s"done $x $y")
      case _ =>
    }

    ftest1()
  }

  def ftest1(): Unit ={
    println("----------------------------------==")
    //((3 + x) + ((x * 2) / x)) ; 2 + x + (x * 2) / x + 2 / 2 + 0
    val parsed = parse("x + y + z + 2 * x - 0 * 5 + 4 * y + 2 * z")
    println(s"parsed: ${show(parsed)}")
    parsed.foreach{ x =>

      val simplified = simplifyUsingEquivRules2(x, combos)
      println("========>")
      println(show(simplified))
    }

    val mat = Matrix(3, 3, Array(
      e"0", e"2", e"1",
      e"6", e"8", e"7",
      e"1", e"2", e"9"
    ))

    val vec = Vector(Array(
      e"1",  e"2" , e"3" ))

    println(matrixToHigherTriangularFormNoZeroChecks(mat, x => x == EInt(0)).simplifyAll().show)
  }


}