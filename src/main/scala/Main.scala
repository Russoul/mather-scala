import cats._
import cats.data._
import cats.implicits._

import scala.reflect.ClassTag


object Main extends App {

  type Bool = Boolean

  implicit class Pipe1[A, R](val a: A) extends AnyVal {
    def |>(f: A => R): R = f(a)
  }

  def impossible[T <: Any] : T = throw new Exception("impossible happened")


  sealed trait BinFn

  case object Plus extends BinFn

  case object Minus extends BinFn

  case object Mult extends BinFn

  case object Div extends BinFn

  sealed trait UnFn

  case object Sqrt extends UnFn

  case object Square extends UnFn

  case object Module extends UnFn

  implicit def showBinFn[T <: BinFn]: Show[T] = {
    case Plus => "+"
    case Minus => "-"
    case Mult => "*"
    case Div => "/"
  }

  implicit def showUnFn[T <: UnFn]: Show[T] = {
    case Sqrt => "Sqrt"
    case Square => "Square"
    case Module => "Abs"
  }


  def isAssoc(fn: BinFn): Bool = {
    fn match {
      case Plus => true
      case Minus => false
      case Mult => true
      case Div => false
    }
  }

  def isCommut(fn: BinFn): Bool = {
    fn match {
      case Plus => true
      case Minus => false
      case Mult => true
      case Div => false
    }
  }

  def isWholeDivision(a: Int, b: Int): Bool = b % a == 0

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

  val expr = EBinFn(EInt(1), EUnFn(EInt(4), Sqrt), Plus)
  val expr1 = EBinFn(EVar("x"), EBinFn(EVar("y"), EVar("z"), Plus), Plus)

  useAssoc(expr1).foreach(x => println(x.show))
  println(simplify(expr,  _ < 2).show)
}