import cats._
import cats.data._
import cats.implicits._
//import cats.syntax._

import scala.reflect.ClassTag
import scala.collection.immutable
import scala.collection.mutable
import Mather._

object Main extends App {

  

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
    e"1", e"2", e"1",
    e"6", e"8", e"7",
    e"1", e"2", e"9 + p"
  ))

  val vec = Vector(Array(
    e"1",  e"2" , e"3" ))

  println(solveLinearSystemSingular(mat, vec).simplifyAll().show)


}