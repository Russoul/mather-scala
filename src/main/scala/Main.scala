import cats._
import cats.data._
import cats.implicits._
import cats.syntax._

import scala.reflect.ClassTag
import scala.collection.immutable
import scala.collection.mutable

object Main extends App {

  type Bool = Boolean

  implicit class Pipe1[A, R](val a: A) extends AnyVal {
    def |>(f: A => R): R = f(a)
  }

  def impossible[T <: Any] : T = throw new Exception("impossible happened")


  sealed trait Assoc
  case object AssocRight extends Assoc
  case object AssocLeft extends Assoc
  case object AssocNone extends Assoc

  sealed trait BinFn

  case object Plus extends BinFn

  case object Minus extends BinFn

  case object Mult extends BinFn

  case object Div extends BinFn

  sealed trait UnFn

  case object Sqrt extends UnFn

  case object Square extends UnFn

  case object Module extends UnFn

  val binFnSyms = immutable.HashMap(Plus -> "+", Minus -> "-", Mult -> "*", Div -> "/")
  val unFnSyms = immutable.HashMap(Sqrt -> "Sqrt", Square -> "Square", Module -> "Abs")

  val binFnAssoc = immutable.HashMap(Plus -> AssocLeft, Mult -> AssocLeft, Minus -> AssocNone, Div -> AssocNone)


  implicit val showBinFn: Show[BinFn] = {
    x => binFnSyms(x.asInstanceOf[BinFn with Product with Serializable])
  }

  implicit val showUnFn: Show[UnFn] = {
    x => unFnSyms(x.asInstanceOf[UnFn with Product with Serializable])
  }

  implicit def showTuple3[A : Show,B : Show,C : Show] : Show[(A,B,C)] = {
    t3 => s"(${t3._1}, ${t3._2}, ${t3._3})"
  }


  def isAssoc(fn: BinFn): Bool = {
    binFnAssoc(fn.asInstanceOf[BinFn with Product with Serializable]) != AssocNone
  }

  def isCommut(fn: BinFn): Bool = {
    fn match {
      case Plus => true
      case Minus => false
      case Mult => true
      case Div => false
    }
  }

  def precedence(fn : BinFn) : Int = {
    fn match{
      case Plus | Minus => 6
      case Mult | Div => 7
    }
  }

  def getAssoc(fn : BinFn) : Assoc = {
    binFnAssoc(fn.asInstanceOf[BinFn with Product with Serializable])
  }

  def isWholeDivision(a: Int, b: Int): Bool = a % b == 0

  def isPerfectSquare(a: Int): Bool = {
    val sqrt = math.sqrt(a)
    val floor = math.floor(sqrt)
    floor * floor == a.toDouble
  }


  /*sealed trait Ty
  case object TyInt extends Ty
  case class TyBinFn[A <: Ty, B <: Ty, F <: BinFn](a : A, b : B, f : F) extends Ty
  case object TyVar extends Ty
  case class TyUnFn[A <: Ty, F <: UnFn](a : A, f : F) extends Ty*/

  sealed trait Expr

  case class EVar(name: String) extends Expr

  case class EInt(value: Int) extends Expr

  case class EBinFn(a: Expr, b: Expr, f: BinFn) extends Expr

  case class EUnFn(a: Expr, f: UnFn) extends Expr

  implicit def showExpr[T <: Expr]: Show[T] = {
    case EInt(x) => if (x >= 0) x.toString else "(" + x.toString + ")"
    case EVar(x) => x
    case EBinFn(x, y, f) => "(" + x.show + " " + f.show + " " + y.show + ")"
    case EUnFn(x, f) => f.show + "(" + x.show + ")"
  }


  def simplifyBinFn(binFn: EBinFn): Expr = {
    binFn match {
      case EBinFn(EInt(x), EInt(y), typee) =>
        typee match {
          case Plus => EInt(x + y)
          case Minus => EInt(x - y)
          case Mult => EInt(x * y)
          case Div => if (isWholeDivision(x, y) && (y != 0)) EInt(x / y) else binFn
        }
      case EBinFn(EVar(x), EVar(y), typee) =>
        if (x == y) {
          typee match {
            case Plus => EBinFn(EInt(2), EVar(y), Mult)
            case Minus => EInt(0)
            case Div => EInt(1)
            case _ => binFn
          }
        } else {
          binFn
        }
      case EBinFn(EBinFn(EInt(k1), EVar(v1), Mult), EBinFn(EInt(k2), EVar(v2), Mult), typee) =>
        //TODO k1 and k2 can be rational numbers
        //(k1 * x) (_) (k2 * y)
        if (v1 == v2){
          typee match{
            case Plus => if(k1 + k2 != 0) EBinFn(EInt(k1 + k2), EVar(v1), Mult) else EInt(0)
            case Minus => if(k1 - k2 != 0) EBinFn(EInt(k1 - k2), EVar(v1), Mult) else EInt(0)
            case Mult => binFn
            case Div => EBinFn(EInt(k1), EInt(k2), Div)
          }
        }else{
          binFn
        }
      case EBinFn(EVar(v1), EBinFn(EInt(k2), EVar(v2), Mult), typee) =>
        //TODO k1 and k2 can be rational numbers
        //(1 * x) (_) (k2 * y)
        if (v1 == v2){
          typee match{
            case Plus => if(1 + k2 != 0) EBinFn(EInt(1 + k2), EVar(v1), Mult) else EInt(0)
            case Minus => if(1 - k2 != 0) EBinFn(EInt(1 - k2), EVar(v1), Mult) else EInt(0)
            case Mult => binFn
            case Div => EBinFn(EInt(1), EInt(k2), Div)
          }
        }else{
          binFn
        }
      case EBinFn(EBinFn(EInt(a), EInt(b), Div), EBinFn(EInt(c), EInt(d), Div), Mult) =>
        //(a/b) * (c/d)
        EBinFn(EInt(a * c), EInt(b * d), Div)
      case EBinFn(EBinFn(EInt(a), EInt(b), Div), EInt(c), Mult) =>
        //(a/b) * c
        EBinFn(EInt(a * c), EInt(b), Div)
      case EBinFn(x, EInt(y), typee) =>
        if (y != 0 || typee == Div) binFn else{
          typee match{
            case Mult => EInt(0)
            case Plus | Minus => x
            case Div => impossible
          }
        }
      case _ => binFn
    }
  }

  def simplifyUnFn(unFn: EUnFn): Expr = {
    unFn match {
      case EUnFn(EInt(x), Sqrt) =>
        if (isPerfectSquare(x))
          EInt(math.sqrt(x).toInt)
        else
          unFn
      case EUnFn(EInt(x), Square) => EInt(x * x)
      case EUnFn(EInt(x), Module) => EInt(math.abs(x))
      case EUnFn(EUnFn(EInt(x), Square), Sqrt) => EUnFn(EInt(x), Module)
      case _ => unFn
    }
  }

  def isSimplifiable(expr: Expr) : Bool = {
    expr match{
      case EInt(_) | EVar(_) => false
      case f@EBinFn(x,y,_) =>
        if (isSimplifiable(x)) true else{
          if(isSimplifiable(y)) true else{
            simplifyBinFn(f) != f
          }
        }
      case f@EUnFn(x,_) =>
        if(isSimplifiable(x)) true else{
          simplifyUnFn(f) != f
        }
    }
  }

  def simplify(expr : Expr, cond : Int => Bool, nat : Int = 0) : (Expr, Int) = {
    if(!cond(nat)) return (expr, nat)


    expr match{
      case EInt(_) => (expr, nat)
      case EVar(_) => (expr, nat)
      case f@EBinFn(x,y,ft) =>
        if(isSimplifiable(x)){
          val inner = simplify(x, cond, nat)
          simplify(EBinFn(inner._1, y, ft), cond, inner._2)
        }else{
          if(isSimplifiable(y)){
            val inner = simplify(y, cond, nat)
            simplify(EBinFn(x, inner._1, ft), cond, inner._2)
          }else{
            if(isSimplifiable(f)){
              val simpl = simplifyBinFn(f) //one simplification

              simplify(simpl, cond, nat + 1)

            }else{
              (expr, nat)
            }
          }
        }
      case f@EUnFn(x, ft) =>
        if(isSimplifiable(x)){
          val inner = simplify(x, cond, nat)
          simplify(EUnFn(inner._1, ft), cond, inner._2)
        }else{
          if(isSimplifiable(f)){
            val simpl = simplifyUnFn(f) //one simplification

            simplify(simpl, cond, nat + 1)
          }else{
            (expr, nat)
          }
        }
    }
  }

  def useAssoc(expr : Expr) : Option[Expr] = {
    expr match{
      case EBinFn(EBinFn(a, b, t1), c, t2) =>
        if(t1 == t2 && isAssoc(t1)){
          Some(EBinFn(a, EBinFn(b, c, t1), t1))
        }else{
          None
        }
      case EBinFn(a, EBinFn(b, c, t1), t2) =>
        if(t1 == t2 && isAssoc(t1)){
          Some(EBinFn(EBinFn(a, b, t1), c, t1))
        }else{
          None
        }
      case _ => None
    }
  }

  private def genEquivAssocL(expr : Expr) : List[EBinFn] = {
    expr match{
      case EBinFn(EBinFn(a,b,t1),c,t2) =>
        if (t1 == t2 && isAssoc(t1)){
          val assoc = EBinFn(a, EBinFn(b,c,t1),t1)
           assoc :: genEquivAssocL(assoc)
        }else
          Nil
      case _ => Nil
    }
  }

  private def genEquivAssocR(expr: Expr) : List[EBinFn] = {
    expr match{
      case EBinFn(a, EBinFn(b,c,t1),t2) =>
        if(t1 == t2 && isAssoc(t2)){
          val assoc = EBinFn(EBinFn(a,b,t1),c,t1)
          assoc :: genEquivAssocR(assoc)
        }else Nil
      case _ => Nil
    }
  }

  def genEquivAssoc(expr: Expr) : List[EBinFn] = genEquivAssocR(expr) ++ genEquivAssocL(expr)

  def genEquivAssocRec(expr: Expr) : List[Expr] = {


    def inner(f : EBinFn) : List[Expr] = {
      val assocs = genEquivAssoc(f)
      val all = f :: assocs
      val list = for((EBinFn(a,b,t3)) <- all) yield{
        val assocAs = genEquivAssoc(a)
        val assocBs = genEquivAssoc(b)
        if(assocAs.isEmpty && assocBs.isEmpty){
          Nil
        }else{
          for(assocA <- assocAs; assocB <- assocBs) yield {
            EBinFn(assocA, assocB, t3)
          }
        }
      }

      all ++ list.flatten
    }

    expr match{
      case f@EBinFn(_, _, _) => inner(f)
      case x => List(x)
    }
  }

  def allLengths[A](list:List[List[A]]) : List[Int] = {
    list map(_.length)
  }

  //TODO hacky way
  def genEquivAssocRecAll(expr : Expr) : List[Expr] = {
    val assocs = genEquivAssocRec(expr)
    val allAssocs = assocs map genEquivAssocRec
    val allAssocsLengths = allLengths(allAssocs)
    val zipped : List[(List[Expr], Int)] = allAssocs.zip(allAssocsLengths)
    val highest : (List[Expr], Int) = zipped.foldRight((Nil : List[Expr], 0)){(prev, acc) => if(acc._2 > prev._2) acc else prev}

    highest._1.distinct
  }


  def genEquivCommutRec(expr: Expr) : List[Expr] = {
    expr match{
      case f@EBinFn(a, b, t) =>
        if (isCommut(t)){
          val com = EBinFn(b,a,t)
          val all = List(f,com)
          val ca = genEquivCommutRec(a)
          val cb = genEquivCommutRec(b)

          if(ca == List(a) && cb == List(b)){
            all
          }else{
            val list = for (a <- ca ; b <- cb) yield List(EBinFn(a,b,t), EBinFn(b,a,t))
            all ++ list.flatten
          }
        }else{
          List(f)
        }
      case x => List(x)
    }
  }

  def genEquivCommutRecAll(expr: Expr): List[Expr] = genEquivCommutRec(expr).distinct

  def genEquivAssocCommutRecAll(expr : Expr) : List[Expr] = {
    val l1 = for(assoc <- genEquivAssocRecAll(expr); commut <- genEquivCommutRecAll(assoc))
      yield commut

    val l2 = for(commut <- genEquivCommutRecAll(expr); assoc <- genEquivAssocRecAll(commut))
      yield assoc

    (l1 ++ l2).distinct
  }




  def simplifyUsingEquivRules(expr: Expr, rules : Expr => List[Expr]) : (Expr, Int) = {
    val vars = rules(expr)
    val simplified = vars.map(x => simplify(x, _ => true))

    def inner(list : List[(Expr,Int)]) : List[(Expr,Int)] = {
      //val simpls = list.filter(_._2 > 0)

      if(list.length <= 1) return list

      val next = list.map(x => simplify(x._1, _ => true, x._2))


      val zipped = next.zip(list)

      val filtered = for((next,prev) <- zipped if next._2 != prev._2) yield next

      if(filtered.isEmpty) return list

      inner(filtered)
    }

    val res = inner(simplified.filter(_._2 > 0))

    if(res.nonEmpty){
      val highest : (Expr, Int) = res.foldRight((null.asInstanceOf[Expr],0)){(prev, acc) => if(acc._2 > prev._2) acc else prev}
      highest
    }else{
      (expr, 0)
    }

  }

  //what it does: "(((*)))" => "*"
  def removeRedundantParenthesis(str : String) : String = {
    if(str.length < 2) return str

    if(str(0) == '(' && str.last == ')') removeRedundantParenthesis(str.substring(1, str.length - 1))
    else str
  }

  def parseBinFn(str : String) : Option[BinFn] = {
    for(sym <- binFnSyms){
      if(sym._2 == str) return Some(sym._1)
    }
    None
  }

  //levels start from 0
  //(1 + 2 * 5) + 3 / 4 - (5/(3+3))
  //in above example level of ^^^ `plus` is `2`
  def findSymLevel(str: String, beginIndex : Int) : Int = {
    var count = 0
    for(i <- 0 until beginIndex){
      if(str(i) == '(') count += 1
      if(str(i) == ')') count -= 1
    }
    count
  }

  def show[T](t : T)(implicit showable : Show[T]) = showable.show(t)
  val putStrLn = (t : String) => println(t)

  def stringHasOnlyDigits(str : String) : Bool = {
    for(char <- str){
      if(char < '0' || char > '9') return false
    }

    str.nonEmpty
  }



  def stringStartsWithDigit(str : String) : Bool = str.nonEmpty && str.head >= '0' && str.last <= '9'

  def stringHasWhitespace(str : String) : Bool = {
    for(char <- str){
      if(char == ' ') return true
    }

    false
  }

  //accepts trimmed input
  def isValidEInt(str : String) : Bool = {
    if(str.isEmpty) return false

    if(str.head != '-'){
      stringHasOnlyDigits(str) && str.head != '0'
    }else{
      stringHasOnlyDigits(str.substring(1)) && str.substring(1).head != '0'
    }
  }

  //accepts trimmed input
  def isValidEVar(str : String) : Bool = {
    !stringStartsWithDigit(str) && str.nonEmpty && !stringHasWhitespace(str)
  }


  def parse(str_ : String) : Option[Expr] = {// (x * 2) + 3


    def make(str_ : String) : Option[Expr] = {
      val str = removeRedundantParenthesis(str_)



      val buf = new mutable.ArrayBuffer[(BinFn, Int, Int)]()

      for(i <- 0 until str.length){
        for(j <- i to str.length){ //to here because substring endIndex may be equal to length
          val tryStr = str.substring(i, j)
          val maybeBinFn = parseBinFn(tryStr)
          maybeBinFn match{
            case Some(f) => if(findSymLevel(str, i) == 0) buf.append((f, i, j))
            case None =>
          }
        }
      }

      if(buf.nonEmpty){
        //TODO debug
        //buf.foreach(putStrLn compose show[(BinFn, Int, Int)])

        var chosen = buf.head
        for(f <- buf){
          if(precedence(f._1) < precedence(chosen._1)){
            chosen = f
          }
        }


        //set correct associativity if multiple associative bin fns are at the same level:
        //x + y + z => (x + y) + z
        if(isAssoc(chosen._1)){
          for(f <- buf){
            if(f._1 == chosen._1){
              getAssoc(chosen._1) match{
                case AssocLeft => if(f._2 > chosen._2) chosen = f
                case AssocRight => if(f._2 < chosen._2) chosen = f
                case _ => impossible
              }
            }
          }
        }

        if(chosen._2 == 0 || chosen._3 == str.length) return None

        val lhs = str.substring(0, chosen._2 - 1)
        val rhs = str.substring(chosen._3 + 1, str.length)

        //TODO debug
        println(s"got string $str")

        //TODO debug
        println(s"op ${chosen}")

        println(s"lhs ${lhs}")
        println(s"rhs ${rhs}")

        val lhsMade = make(lhs)
        val rhsMade = make(rhs)

        println(s"lhsm ${lhsMade} ${lhs}")
        println(s"rhsm ${rhsMade} ${rhs}")

        for(lhs <- lhsMade; rhs <- rhsMade) yield EBinFn(lhs, rhs, chosen._1)

        //...
      }else{ //x or 12345
        val trimmed = str.trim
        //TODO debug
        println(trimmed)
        if(isValidEInt(trimmed)){
          //TODO debug
          println("EInt:" + trimmed)
          Some(EInt(Integer.parseInt(trimmed)))
        }
        else if(isValidEVar(trimmed)){
          Some(EVar(trimmed))
        }else
          None
      }
    }


    make(str_)

  }


  val expr = EBinFn(EInt(1), EUnFn(EInt(4), Sqrt), Plus)
  val expr1 = EBinFn(EVar("x"), EBinFn(EVar("y"), EVar("z"), Plus), Plus)


  val exp0 = EBinFn(EInt(10), EInt(5), Mult)
  val exp1 = EBinFn(EInt(1), exp0, Plus)
  val exp2 = EBinFn(exp1, EInt(2), Minus)
  val exp3 = EUnFn(exp2, Sqrt)
  val exp4 = EBinFn(exp3, EVar("x"), Plus)
  val exp5 = EBinFn(exp4, EVar("x"), Plus)

  println(s"exp5 = ${exp5.show}")
  println("exp5 simplified = " ++ simplifyUsingEquivRules(exp5, genEquivAssocCommutRecAll).show)

  useAssoc(expr1).foreach(x => println(x.show))
  println(simplify(expr,  _ < 2).show)

  genEquivAssocCommutRecAll(expr1).foreach(x => println(x.show))

  println("----------------------------------")
  parse("1 * 2 + (3 + 5) + 15 / 5").foreach{ x =>
    val simplified = simplify(x, _ => true) //TODO simplify works incorrectly
    println(show(x))
    println("========>")
    println(show(simplified))
  }
}