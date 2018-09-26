package me.russoul.mather

import cats._
import cats.implicits._
import NewParser._
import Mather._
import Helper._

object Main {



  //TODO mark debug message that they are debug messages
  //TODO add simplification rule : x * 1 == x
  //TODO make sure we do not have equivalent simplification rules, e.x: 0 * x == x * 0 == 0
  //TODO dynamic refinements are good to be used here for debug and for self explaned parameter constraints(port my scalac plugin for this to dotty ?)
  //TODO matrices (as expressions)
  //TODO domain of an Expr(== domain of a function it represents)
  //TODO upgrade parser, use parser combinators
  //TODO don't show redundant parenthesis when pretty printing expression
  //TODO add pow, make it work with current Square and Sqrt


  //TODO uniform system for defining un and bin functions and un and bin operators

  //TODO a way to say that a function `f` is dependent on some argument(saying that a function depends on x implies that it does not depend on any other variable)
  //TODO if x is a function itself ^^ than f depends on the arguments of x automatically

  //TODO a way to do the ^^^ is to add a special environment that for each variable(function) tells if it depends on any other or it is unbound(independent)

  //def matrixMultiply(a : Matrix, b : Matrix)(ref:R[a.m == b.n]) ==>
  //def matrixMultiply(a : Matrix, b : Matrix){
  //  assume(a.m == b.n)
  //  ...
  //}


  def main(args : Array[String]) : Unit = {
    //ftest3()

    val e = e"pow(t, 3)/3 - 13*pow(t, 2) + 168 * t"
    println(simplifyUsingEquivRules2Stages(substitute(e, e"t".asInstanceOf[EVar], EInt(14)), combos).show)
    val subs = (i : Expr) => substitute(e, EVar("t"), i)
    val res = e"2 * ${subs(e"12")} - 2 * ${subs(e"14")} + ${subs(e"15")} - ${subs(e"0")}"
    println(simplifyUsingEquivRules2(res, combos)._1.show)
  }

  def ftest3(): Unit ={

    val t =
      List(
        "u = u(x,y)",
        "d/dx(u + x + y)"

      ).map(x => parse(x))

    if(t.forall(x => x.successful)){
      println(makeScope(t.map(x => x.get)))
    }

  }

  def ftest2(): Unit ={


    val simplify = (x:Expr) => simplifyUsingEquivRules2(x, combos)._1
    val isZero = (x:Expr) => x == EInt(0)

    val tr = parse("dif_t(u + 2 * t + 5 * sqrt(t))")
    println("parsed: " + tr.successful)
    tr.map{x =>
      println("parsed: " + x.show)
      val e1 = simplify(x)
      println("e1 = " + e1.show)
      val e2 = simplify(substitute(e1, EVar("u"), e"gzi + nu"))
      println("e2 = " + e2.show)
    }

    import Helper._


    val tr2 = solveQuadraticEq(e"a", e"2", e"1").map(simplify)
    println(tr2.show)
  }

  def ftest1(): Unit ={
    println("----------------------------------==")
    //((3 + x) + ((x * 2) / x)) ; 2 + x + (x * 2) / x + 2 / 2 + 0
    val parsed = parse("1 / 2 / 3") //((47 / (-2)) + 1) / 3
    println(s"parsed: ${show(parsed)}")

    parsed.map{ x =>
      println("simplifiable: " + isSimplifiable(parsed.get))
      val simplified = simplifyUsingEquivRules2(x, combos)
      println("========>")
      println(show(simplified))
    }

    val mat = Matrix(3, 3, Array(
      e"0", e"2", e"1",
      e"0", e"8", e"4",
      e"0", e"4", e"2"
    ))

    val vec = Vector(Array(
      e"1",  e"2" , e"3" ))


    val simplify = (x:Expr) => simplifyUsingEquivRules2(x, combos)._1
    val isZero = (x:Expr) => x == EInt(0)

    //println(solveLinearSystemSingular(mat, vec, x => x == EInt(0)).simplifyAll().show)
    println("rank: " + matrixRank(mat, isZero, simplify))

    println("are col: " + areCollinear(Vector(Array(EInt(0),EInt(2), EInt(1), EInt(2))), Vector(Array(EInt(0),EInt(1), EBinFn(EInt(1), EInt(2), Div), EInt(0))), isZero, simplify))
  }


}