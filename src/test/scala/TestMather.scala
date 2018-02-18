import cats.Show
import cats.implicits._
import me.russoul.mather.Mather._

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import me.russoul.mather.Parser._

object TestMather{

  @elidable(ASSERTION)
  def assertEqualShow[A : Show](a : A, b : A) : Unit = {
    val res = a == b
    if(!res){
      println("lhs: " + a.show)
      println("rhs: " + b.show)
      throw new java.lang.AssertionError("assertion failed")
    }
  }

  def testParsec() : Unit = {
    import me.russoul.mather.NewParser._
    import scala.util.parsing.combinator.Parsers


    val result1 = parseAll(parseExpr, "-1 + 2")
    assert(result1.successful && result1.get == EBinFn(EInt(-1),EInt(2),Plus))


    val result2 = parseAll(parseExpr, "a + b + c")
    assert(result2.successful && result2.get == EBinFn(EBinFn(EVar("a"),EVar("b"),Plus),EVar("c"),Plus))

    val result3 = parseAll(parseExpr, "sqrt(1 - 2 * sin(-x))")
    assertEqualShow(result3.get, EUnFn(EBinFn(EInt(1),EBinFn(EInt(2),EUnFn(EBinFn(EInt(-1),EVar("x"),Mult),Sin),Mult),Minus),Sqrt))


    val result4 = parseAll(parseExpr, "-sqrt(0)")
    assertEqualShow(result4.get, EBinFn(EInt(-1),EUnFn(EInt(0),Sqrt),Mult))

    val result5 = parseAll(parseExpr, "sin(x * cos(y) * abs(z) * -pi / 4)")
    assertEqualShow(result5.get, EUnFn(EBinFn(EBinFn(EBinFn(EBinFn(EVar("x"),EUnFn(EVar("y"),Cos),Mult),EUnFn(EVar("z"),Module),Mult),EBinFn(EInt(-1),EConst("pi"),Mult),Mult),EInt(4),Div),Sin))

    val result6 = parseAll(parseExpr, "1 + (2 + 3)")
    assertEqualShow(result6.get, EBinFn(EInt(1),EBinFn(EInt(2),EInt(3),Plus),Plus))

    val result8 = parseAll(parseExpr, "(cos(x) * sin(y))")
    assertEqualShow(result8.get, EBinFn(EUnFn(EVar("x"),Cos),EUnFn(EVar("y"),Sin),Mult))

    val result7 = parseAll(parseExpr, "-d/dx  (x + 2*square(x))") //d/d...\s*(...), d/d... part must be have no whitespace
    assertEqualShow(result7.get, EBinFn(EInt(-1), EUnFn(EBinFn(EVar("x"),EBinFn(EInt(2),EUnFn(EVar("x"),Square),Mult),Plus),Dif(EVar("x"))), Mult))

  }

  def testAll() : Unit = {
    assert(e"pi" == EConst("pi"))
    assert(e"x" == EVar("x"))
    assert(isValidEInt("10"))
    assert(isValidEInt("-1"))
    assert(isValidEVar("x"))
    assert(isValidEVar("x1x"))
    assert(!isValidEVar("-x"))
    assert(parseBinFn("+", "1+1").isDefined)
    assert(stringStartsWithDigit("1"))
    assert(!stringStartsWithDigit("z1"))
    assert(!isValidEVar("1x1x"))
    assert(parseBinFn("-", "1 - 1").isDefined)
    assert(parseBinFn("-", "- 1").isEmpty)
    assert(parseUnFn("sqrt(1)").isDefined)
    assert(parseUnFn("sqrt(1) ").isEmpty)
    assert(parseUnFn("sqrt   (t + 2)").isDefined)
    assert(parseUnFn("sqrt1(1)").isEmpty)
    assert(parse("1 - 1") == Some(EBinFn(EInt(1), EInt(1), Minus)))
    assert(parse("a - b") == Some(EBinFn(EVar("a"), EVar("b"), Minus)))
    assert(parse("  -1  ") == Some(EInt(-1)))
    assert(parse(" - 1 ") == None)
    assert(parse("a + b + c") == Some(EBinFn(EBinFn(EVar("a"), EVar("b"), Plus), EVar("c"), Plus)))
    assert(parse("a - b + c") == Some(EBinFn(EBinFn(EVar("a"), EVar("b"), Minus), EVar("c"), Plus)))
    assertEqualShow(parse("a - b - c") , Some(EBinFn(EBinFn(EVar("a"), EVar("b"), Minus), EVar("c"), Minus)) )
    assertEqualShow(parse("sqrt( 2 + 5 - 10 + abs( -1 ) )") , Some(EUnFn(EBinFn(EBinFn(EBinFn(EInt(2), EInt(5), Plus), EInt(10), Minus), EUnFn(EInt(-1), Module), Plus), Sqrt)))

    assertEqualShow(simplify(e"2 - 4/3", _ => true)._1 , e"2/3")


    assert(e"1 + 2" match{
      case e"$x + 2" => x == EInt(1)
      case _ => false
    })

    assert(e"abs(sqrt(5 + y))" match{
      case e"abs(sqrt($five + $y))" => five == EInt(5) && y == EVar("y")
      case _ => false
    })


    assert(e"2 + sqrt(sqrt(3) / 2)" match{
      case e"2+sqrt($x/2)" => x == EUnFn(EInt(3), Sqrt)
      case _ => false
    })


    val simplifyFull = (e : Expr) => simplifyUsingEquivRules2(e, combos)
    val simplifyOneStep = (e : Expr) => simplify(e, _ < 5)._1

    e"1 + 1"
    e"2 * 1 + 5 * sqrt(5)" //<-- those will throw None.get exception if their expressions can not be parsed
    e"-1 * x + y"
    e"1+2"
    assert(simplifyOneStep(e"sin(pi/4)") == e"sqrt(2)/2")
    println(simplifyFull(e"sin(x) - sin(x)").show) //TODO output is incorrect, debug

    //TODO `-x` cannot be parsed use `-1 * x` for now
    //TODO test sin simplifications
    //TODO test all simplifications

    //TODO simplification rules
    //TODO equivalence rules

  }

  def main(args : Array[String]) : Unit = {
    testAll()
    testParsec()
  }
}