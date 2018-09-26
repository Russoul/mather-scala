package me.russoul.mather

import cats._
import cats.implicits._
import NewParser._

import scala.collection.immutable.HashMap
import scala.util.control.Breaks
//import cats.syntax._

import scala.collection.{immutable, mutable}

object Mather {

  type Bool = Boolean

  implicit class Pipe1[A, R](val a: A) extends AnyVal {
    def |>(f: A => R): R = f(a)
  }

  def impossible[T <: Any] : T = throw new Exception("impossible happened")


  case class FuncDef(binding : EVar, expr : Either[Expr, List[EVar]]) extends Expr//is binding, expr is either valid expr that may contain some binding or a list of bindings themselves
  //e.g 1)f = 2 * x + y, FuncDef(EVar(f), Left(2 * x + y))
  //    2)f = f(gzi, eta), FuncDef(Evar(f), Right(gzi, eta))    in this case function is unknown, but we still know the bindings

  case class FuncEnv(env : mutable.HashMap[EVar, List[EVar]]) //this class contains dependencies of variables; variable may be dependent or independent
  //e.g: f(x) = x, in this function x is independent variable, f depends on x
  //but
  //f(x, x(y)) = x, in this function x depends on y, y is independent, f explicitly depends on x and implicitly on y
  //FuncEnv must be defined per scope, so it covers multiple expressions


  //makeScope function should be used to make a new Scope
  //defs may be Nil(then env will also contain no bindings)
  case class Scope(defs : List[FuncDef], expr : Expr, env : FuncEnv)
  //u = u(gzi, eta) //u is a function of gzi and eta, it is unknown, but we can do useful stuff with it, like differentiation


  //f = pow(x,2)                           <- f automatically infered as f(x)
  //y = 2 * x
  //g = y + 2 * x + 1                      <- g infered as g(x,y(x))
  //d/dx(g)              <- last expr

  //what do we get ?
  //x - independent
  //y - is function of x            <- this info is stored in `env`
  //f is function of x
  //g is function of x and y

  //order matters

  //the following is incorrect:

  //g = y + 2 * x + 1         //this line tells us that y and x are independent
  //y = 2 * x                 //this line breaks the independence


  //example:

  //gzi = gzi(x,y)
  //eta = eta(x,y)

  //u = u(gzi, eta)
  //d/dx(u)

  //the above ^^ is actually a system of equations

  //scope is just a sequence of variable(function) definitions + last expr that represents the result of calculation
  //those must not intervene or produce cyclic references(relations between each other)

  //notational associativity
  sealed trait Assoc
  case object AssocRight extends Assoc
  case object AssocLeft extends Assoc
  //case object AssocNone extends Assoc

  sealed trait BinFn

  case object Plus extends BinFn

  case object Minus extends BinFn

  case object Mult extends BinFn

  case object Div extends BinFn

  case object Pow extends BinFn


  sealed trait UnFn

  case object Sqrt extends UnFn //wont ever get parsed, will be parsed as pow(x,1/2) (in the new parser)

  case object Module extends UnFn

  case object Sin extends UnFn

  case object Cos extends UnFn

  case class Dif(variable : EVar) extends UnFn

  val binOpSyms = immutable.HashMap(Plus -> "+", Minus -> "-", Mult -> "*", Div -> "/")
  val binFnSyms = immutable.HashMap(Pow -> "pow")
  def keyForValue[Key, Value](hashMap: HashMap[Key, Value], value : Value) : Option[Key] = {
    for( (k,v) <- hashMap ){
      if(v == value) return Some(k).asInstanceOf[Option[Key]]
    }

    None
  }

  //those are used as `default` function notation, e.g: sqrt(1) or square(2)
  val unFnSyms = immutable.HashMap(Sqrt -> "sqrt", Module -> "abs", Sin -> "sin", Cos -> "cos")

  //this is `default` (notational) associativity, used when parsing expr from string, e.g: a + b + c will be parsed as (a + b) + c
  //TODO not used yet inside new parser
  val binOpAssoc = immutable.HashMap(Plus -> AssocLeft, Mult -> AssocLeft, Minus -> AssocLeft, Div -> AssocLeft)

  //list of all constants
  //when something like `x + pi` is parsed if symbol `x` is contained inside the below list it is parsed as a constant (in case of `x` it is not)
  //if it not then like a variable
  val constants = immutable.List("pi", "e", "i")


  implicit val showBinFn: Show[BinFn] = {
    case Pow => "pow"
    case x => binOpSyms(x.asInstanceOf[BinFn with Product with Serializable])
  }

  implicit val showUnFn: Show[UnFn] = {
    case x if !x.isInstanceOf[Dif] => unFnSyms(x.asInstanceOf[UnFn with Product with Serializable])
    case x: Dif => "d/d"+x.variable.name
  }



  //TODO isAssoc, isCommut, precedence must only be used with operators and not functions(like pow)
  def isAssoc(fn: BinFn): Bool = {
    fn match {
      case Plus => true
      case Minus => false
      case Mult => true
      case Div => false
      case Pow => false
    }
  }

  def isCommut(fn: BinFn): Bool = {
    fn match {
      case Plus => true
      case Minus => false
      case Mult => true
      case Div => false
      case Pow => false
    }
  }

  def precedence(fn : BinFn) : Int = {
    fn match{
      case Plus | Minus => 6
      case Mult | Div => 7
    }
  }

  //returns notational associativity
  def getAssoc(fn : BinFn) : Assoc = {
    fn match{
      case Pow => throw new Exception("not operator")
      case _ => binOpAssoc(fn.asInstanceOf[BinFn with Product with Serializable])
    }
  }

  def isWholeDivision(a: Int, b: Int): Bool = if(b == 0)true else a % b == 0

  def isPerfectSquare(a: Int): Bool = {
    if(a < 0) return false

    val sqrt = math.sqrt(a)
    val floor = math.floor(sqrt)
    floor * floor == a.toDouble
  }

  def GCD(a: Int, b: Int): Int = {
    if (b == 0) return a
    GCD(b, a % b)
  }

  //non negative integer power
  def upow(a : Int, n : Int) : Int = {
    n match{
      case 1 => a
      case 0 => 1
      case _ => a * upow(a, n - 1)
    }
  }

  def isConst(expr : Expr) : Bool = { //returns true if the function returns constant value for all inputs(this actually means that there are no variables inside its tree)
    expr match{
      case EInt(_) | EConst(_) => true
      case EVar(_) => false
      case EUnFn(x, _) => isConst(x)
      case EBinFn(x,y,_) => isConst(x) && isConst(y)
    }
  }




  //TODO VERY IMPORTANT NOTICE !!!!!!!
  //In order to simplify things A LOT + make them mathematically (theoretically) correct
  //we assume that every expression IS a FUNCTION
  //i.e
  //EInt(1) is a function of unknown number of arguments that returns a constant value `1` at each point of its domain
  //EConst(name) is a function of unknown number of arguments that returns some unknown constant value (or parameter if you will) at each point of its domain
  //EVar(name) is a function of one argument - `name`, which is equal to its variable at each point of its domain
  //notice about domains:
  //we should somehow create a notation to express a domain of a function
  //EUnFn and EBinFn are complex functions (functions of functions) TODO write more about complex functions
  sealed trait Expr

  case class EVar(name: String) extends Expr

  case object EInf extends Expr //infinity

  case class EInt(value: Int) extends Expr

  case class EConst(name : String) extends Expr

  case class EBinFn(a: Expr, b: Expr, f: BinFn) extends Expr

  case class EUnFn(a: Expr, f: UnFn) extends Expr

  implicit def showExpr[T <: Expr]: Show[T] = {
    case EInt(x) => if (x >= 0) x.toString else "(" + x.toString + ")"
    case EConst(x) => x
    case EVar(x) => x
    case EInf => "inf"
    case EBinFn(EInt(-1), EInf, Mult) => "-inf"
    case EBinFn(EInf, EInt(-1), Mult) => "-inf"
    case EBinFn(x, y, f) =>
      f match{
        case Pow => f.show + "(" + x.show + ", " + y.show + ")"
        case _ => "(" + x.show + " " + f.show + " " + y.show + ")"
      }
    case EUnFn(x, f) => f.show + "(" + x.show + ")"

  }


  def simplifyBinFn(binFn: EBinFn): Expr = {
    val ret = binFn match {
      case EBinFn(EInt(x), EInt(y), typee) =>             //<--------  Int # Int
        typee match {
          case Plus => EInt(x + y)
          case Minus => EInt(x - y)
          case Mult => EInt(x * y)
          case Div => if (isWholeDivision(x, y) && (y != 0)) EInt(x / y) else {
            if(y == 0) return binFn

            def simpl(x : Int, y : Int) : (Int, Int) = {
              val gcd = GCD(x,y)
              if(gcd == 1) return (x, y)

              simpl(x / gcd, y / gcd)
            }

            val gcdProc = simpl(x, y)

            if(gcdProc._1 < 0 && gcdProc._2 < 0) EBinFn(EInt(-gcdProc._1), EInt(-gcdProc._2), Div) else EBinFn(EInt(gcdProc._1), EInt(gcdProc._2), Div)

          }
          case Pow =>
            if(y >= 0)
              EInt(upow(x, y))
            else
              EBinFn(EInt(1), EInt(upow(x, y)), Div)
        }
      case EBinFn(EInt(x), EBinFn(EInt(y), EInt(z),op), Mult) if op == Plus || op == Minus => //distributive property
        EBinFn(EInt(x * y), EInt(x * z), op)
      case EBinFn(a, EBinFn(b, c,Div), Mult) =>
        EBinFn(EBinFn(a,b,Mult), c, Div)
      case EBinFn(EInt(x), EBinFn(EInt(y), EInt(z), Div), op) if op == Plus || op == Minus => EBinFn(EBinFn(EInt(x * z), EInt(y), op), EInt(z), Div)
      case EBinFn(EBinFn(EInt(a), EInt(b), Div), EInt(c), Div) if b != 0 && c != 0 => EBinFn(EInt(a), EInt(b * c), Div)


      case EBinFn(EVar(x), EVar(y), typee) =>          //<---------- x # y
        if (x == y) {
          typee match {
            case Plus => EBinFn(EInt(2), EVar(y), Mult)
            case Minus => EInt(0)
            case Div => EInt(1)
            case _ => binFn
          }
        } else binFn
      case EBinFn(EConst(x), EConst(y), typee) =>          //<---------- x # y
        if (x == y) {
          typee match {
            case Plus => EBinFn(EInt(2), EVar(y), Mult)
            case Minus => EInt(0)
            case Div => EInt(1)
            case _ => binFn
          }
        } else binFn
      case EBinFn(EBinFn(EInt(k1), v1, Mult), EBinFn(EInt(k2), v2, Mult), typee) =>
        //TODO k1 and k2 can be rational numbers
        //(k1 * x) # (k2 * y)
        if (v1 == v2 && isConst(v1)){//if v1 == v2 and are constants we can safely performance the following ops:
          typee match{
            case Plus => if(k1 + k2 != 0) EBinFn(EInt(k1 + k2), v1, Mult) else EInt(0)
            case Minus => if(k1 - k2 != 0) EBinFn(EInt(k1 - k2), v1, Mult) else EInt(0)
            case Mult => binFn
            case Div => EBinFn(EInt(k1), EInt(k2), Div)
          }
        }else binFn
      case EBinFn(v1, EBinFn(EInt(k2), v2, Mult), typee) =>
        //TODO k1 and k2 can be rational numbers
        //x # (k2 * y)
        if (v1 == v2 && isConst(v1)){
          typee match{
            case Plus => if(1 + k2 != 0) EBinFn(EInt(1 + k2), v1, Mult) else EInt(0)
            case Minus => if(1 - k2 != 0) EBinFn(EInt(1 - k2), v1, Mult) else EInt(0)
            case Mult => binFn
            case Div => EBinFn(EInt(1), EInt(k2), Div)
          }
        }else{
          binFn
        }

      case EBinFn(EBinFn(x, EInt(a), Div), EInt(b), Mult) if a == b && a != 0 => x //  x / a * a == x if a != 0
      case EBinFn(EBinFn(x, EInt(a), Mult), EInt(b), Div) if a == b && a != 0 => x //  x * a / a == x if a != 0

      case EBinFn(EBinFn(EInt(a), EInt(b), Div), EBinFn(EInt(c), EInt(d), Div), Mult) =>
        //(a/b) * (c/d)
        EBinFn(EInt(a * c), EInt(b * d), Div)



      //a + b/c //where c is a number != 0
      case EBinFn(a, EBinFn(b,EInt(c),Div),Plus) if c != 0 => EBinFn( EBinFn( EBinFn(a, EInt(c), Mult), b, Plus ), EInt(c), Div )
      //a - b/c //where c is a number != 0
      case EBinFn(a, EBinFn(b,EInt(c),Div),Minus) if c != 0 => EBinFn( EBinFn( EBinFn(a, EInt(c), Mult), b, Minus ), EInt(c), Div )



        //a/b +- c
      case EBinFn(EBinFn(a, b, Div), c@EInt(x), typee) if (typee == Plus || typee == Minus) && x != 0 =>
        EBinFn(EBinFn(a, EBinFn(c, b, Mult), typee), b, Div)
      case EBinFn(a, EBinFn(EInt(b), EInt(c), Div), Div) if c != 0 && b != 0 => //   a / b / c where b and c numbers != 0
        EBinFn(EBinFn(a, EInt(c), Mult), EInt(b), Div)
      case EBinFn(EBinFn(EInt(a), EInt(b), Div), EInt(c), Mult) =>
        //(a/b) * c
        EBinFn(EInt(a * c), EInt(b), Div)
      case EBinFn(a, EInt(x), Div) if x == 1 => a // a / 1
      case EBinFn(EBinFn(EInt(minusOne), x, Mult), EBinFn(EInt(minusOne2), y, Mult), Mult) if minusOne == minusOne2 && minusOne == -1 => // -a * (-b)
        EBinFn(x,y,Mult)
      case EBinFn(EBinFn(EInt(minusOne), x, Mult), EBinFn(EInt(minusOne2), y, Mult), Div) if minusOne == minusOne2 && minusOne == -1 => // -a / (-b)
        EBinFn(x,y,Div)
      case EBinFn(x, EInt(y), typee) =>
        if (y != 0 || typee == Div) binFn else{
          typee match{
            case Mult => EInt(0)
            case Plus | Minus => x
            case Div => impossible
          }
        }
      case EBinFn(EInt(0), y, typee) =>
        typee match{
          case Mult => EInt(0)
          case Plus => y
          case Minus => EBinFn(EInt(-1), y, Mult)
          case Div => EInt(0)
        }

      case EBinFn(EInt(x), EBinFn(EInt(1), EInt(2), Mult), Pow) =>
        if(isPerfectSquare(x))
          EInt(math.sqrt(x).toInt)
        else
          binFn

      case _ => binFn
    }
    ret
  }


  def simplifyDifOp(op : Dif, expr : Expr) : Expr = {
    expr match{
      case EUnFn(arg, Sin) => EBinFn(EUnFn(arg, Cos), EUnFn(arg, op), Mult)
      case EUnFn(arg, Cos) => EBinFn(EBinFn( EInt(-1), EUnFn(arg, Sin), Mult ), EUnFn(arg, op), Mult)
      case EBinFn(arg, pow, Pow) if isConst(pow) => EBinFn(pow, EBinFn(arg, EBinFn(pow, EInt(1), Minus), Pow), Mult)
      case EBinFn(x, y, plusMinus) if plusMinus == Plus || plusMinus == Minus => EBinFn( EUnFn(x, op), EUnFn(y, op), plusMinus)
      case EInt(_) | EConst(_) => EInt(0)
      case EVar(sym) =>
        if(sym == op.variable.name)
          EInt(1)
        else
          EUnFn(expr,op)
      case EBinFn(k, f, Mult) if isConst(k) =>
        EBinFn(k, EUnFn(f, op), Mult)
      case EBinFn(EVar(s1), EVar(s2), Mult) => EBinFn(EBinFn(EUnFn(EVar(s1), op), EVar(s2), Mult), EBinFn(EVar(s1), EUnFn(EVar(s2), op), Mult), Plus)
    }
  }

  def simplifyUnFn(unFn: EUnFn): Expr = {
    unFn match {
      case EUnFn(EInt(x), Module) => EInt(math.abs(x))

      case EUnFn(expr, dif@Dif(_)) => simplifyDifOp(dif, expr)


      case EUnFn(x, Sin) =>
        x match{
          case EInt(0) | EConst("pi") => EInt(0)
          case EBinFn(EConst("pi"),EInt(2),Div) => EInt(1)
          case EBinFn(EBinFn(EInt(3), EConst("pi"), Mult), EInt(2), Div ) => EInt(-1)
          case EBinFn(EConst("pi"), EInt(4), Div) | EBinFn(EBinFn(EInt(3), EConst("pi"), Mult), EInt(4), Div ) => EBinFn(EBinFn(EInt(2), EBinFn(EInt(1), EInt(2), Div),Pow),EInt(2),Div)
          case EBinFn(EConst("pi"), EInt(6), Div) | EBinFn(EBinFn(EInt(5), EConst("pi"), Mult), EInt(6), Div ) => EBinFn(EInt(1), EInt(2), Div)
          case EBinFn(EConst("pi"), EInt(3), Div) | EBinFn(EBinFn(EInt(2), EConst("pi"), Mult), EInt(3), Div ) => EBinFn(EBinFn(EInt(3), EBinFn(EInt(1), EInt(2), Div),Pow),EInt(2),Div)
          case EBinFn(EBinFn(EInt(7), EConst("pi"), Mult), EInt(6), Div ) | EBinFn(EBinFn(EInt(11), EConst("pi"), Mult), EInt(6), Div ) => EBinFn(EInt(-1), EInt(2), Div)
          case EBinFn(EBinFn(EInt(5), EConst("pi"), Mult), EInt(4), Div ) | EBinFn(EBinFn(EInt(7), EConst("pi"), Mult), EInt(4), Div ) => EBinFn(EBinFn(EInt(2), EBinFn(EInt(1), EInt(2), Div),Pow),EInt(-2),Div)
          case EBinFn(EBinFn(EInt(4), EConst("pi"), Mult), EInt(3), Div ) | EBinFn(EBinFn(EInt(5), EConst("pi"), Mult), EInt(3), Div ) => EBinFn(EBinFn(EInt(3), EBinFn(EInt(1), EInt(2), Div),Pow),EInt(-2),Div)
          case _ => unFn
        }


      case _ => unFn
    }
  }

  def isSimplifiable(expr: Expr) : Bool = {
    val ret = expr match{
      case EInt(_) | EVar(_) | EConst(_) | EInf => false
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

    ret
  }

  def simplify(expr : Expr, cond : Int => Bool, nat : Int = 0) : (Expr, Int) = {
    if(!cond(nat)) return (expr, nat)


    expr match{
      case EInt(_) | EVar(_) | EConst(_) | EInf => (expr, nat)
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


  //Rules we have so far
  def useAssocL(expr : Expr) : Option[Expr] = {
    expr match{
      case EBinFn(EBinFn(a, b, t1), c, t2) =>
        if(t1 == t2 && isAssoc(t1)){
          Some(EBinFn(a, EBinFn(b, c, t1), t1))
        }else{
          None
        }
      case _ => None
    }
  }

  def useAssocR(expr : Expr) : Option[Expr] = {
    expr match{
      case EBinFn(a, EBinFn(b, c, t1), t2) =>
        if(t1 == t2 && isAssoc(t1)){
          Some(EBinFn(EBinFn(a, b, t1), c, t1))
        }else{
          None
        }
      case _ => None
    }
  }

  def useCommut(expr: Expr) : Option[Expr] = {
    expr match {
      case EBinFn(a, b, op) if isCommut(op) => Some(EBinFn(b, a, op))
      case _ => None
    }
  }
  //=========================================================================

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
          for(assocA <- genEquivAssocRec(a); assocB <- genEquivAssocRec(b)) yield {
            EBinFn(assocA, assocB, t3)
          }
        }
      }

      (all ++ list.flatten).distinct
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
          for (a <- genEquivCommutRec(a) ; b <- genEquivCommutRec(b)) yield EBinFn(a,b,t) //TODO append vice versa commutation ?
        }
      case x => List(x)
    }
  }

  def genEquivCommutRecAll(expr: Expr): List[Expr] = genEquivCommutRec(expr).distinct


  //this function generates all possible valid combinations of given expression, valid is a sense that all generated combos are equivalent to the given one
  //equivalent combos are generated from two rules : commutativity and associativity
  def genEquivAssocCommutRecAll(expr : Expr) : List[Expr] = {
    val l1 = for(assoc <- genEquivAssocRecAll(expr); commut <- genEquivCommutRecAll(assoc))
      yield commut

    val l2 = for(commut <- genEquivCommutRecAll(expr); assoc <- genEquivAssocRecAll(commut))
      yield assoc

    (l1 ++ l2).distinct
  }




  /*def simplifyUsingEquivRules1(expr_ : Expr, rules : Expr => List[Expr]) : (Expr, Int) = {


    //add presimplification for performance
    val (expr,steps) = simplify(expr_, _ => true)

    val vars = rules(expr)
    val simplified = vars.map(x => simplify(x, _ => true, steps))



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

  }*/

  def simplifyUsingEquivRules2Old(expr_ : Expr, rules : Expr => List[Expr]) : (Expr, Int) = {


    val always = (_ : Int) => true

    def perf(expr: Expr, steps : Int) : (Expr, Int) = {
      //add presimplification for performance
      println(s"pre simple")
      val (out,newSteps) = simplify(expr, always, steps)

      if(newSteps > steps){
        println(s"first go")
        perf(out, newSteps)
      }else{
        println(s"pre gen")
        val vars = rules(out)
        println(s"post gen")

        var i = 0
        for(variant <- vars){
          i += 1
          val simpl = simplify(variant, always, newSteps)
          if(simpl._2 > newSteps){
            return perf(simpl._1, simpl._2)
          }
          println(s"checking $i out of ${vars.size}")
        }

        (out, newSteps)
      }

    }

    println("in")
    val res = perf(expr_, 0)
    println("done")
    res



  }

  def makeCombosOfRules(rules : Stream[Stream[Expr => Option[Expr]]]) : Stream[Expr => Option[Expr]] = {
    val eachOne = rules.flatten

    val res = for(list <- rules; fromEach <- list) yield{ //take rule from each list
      for(list <- rules if !list.contains(fromEach) ; elem <- list) yield{ //apply it with each other one not from the same list
        (expr : Expr) => fromEach(expr) match{
          case Some(expr) => elem(expr)
          case None => None
        }
      }
    }

    eachOne ++ res.flatten

  }

  def applyRules(expr : Expr, ruleCombos : Stream[Expr => Option[Expr]]) : Stream[Expr] = {
    expr match{
      case s@(EInt(_) | EConst(_) | EVar(_) | EInf) => s #:: ruleCombos.flatMap(rule => rule(s))
      case f@EUnFn(arg, op) =>
        val inner = applyRules(arg, ruleCombos)
        inner.map(x => EUnFn(x, op)) ++ inner.flatMap(
          a => ruleCombos.flatMap(
            rule => rule(EUnFn(a, op))))

      case f@EBinFn(a, b, op) =>
        val inner1 = applyRules(a, ruleCombos)
        val inner2 = applyRules(b, ruleCombos)


        (for(x <- inner1; y <-inner2) yield EBinFn(x,y,op)) ++ inner1.flatMap(
          a => inner2.flatMap(
            b => ruleCombos.flatMap(
              rule => rule(EBinFn(a,b,op))
            )
          )
        )


    }
  }

  //trying to apply given rule to expression recursively(if it cannot apply the rule to given expr it applies it to its children); return after the first success
  def applyRuleRec(expr : Expr, ruleCombo : Expr => Option[Expr]) : Option[Expr] = {
    expr match{
      case s@(EInt(_)|EVar(_)|EConst(_)|EInf) => ruleCombo(s)
      case f@EUnFn(arg, op) => ruleCombo(f) match{
        case ok@Some(_) => ok
        case None => for(ok <- applyRuleRec(arg, ruleCombo)) yield EUnFn(ok, op)
      }
      case f@EBinFn(a, b, _) => ruleCombo(f) match{
        case ok@Some(_) => ok
        case None => applyRuleRec(a, ruleCombo) match {
          case ok@Some(_) => ok
          case None => applyRuleRec(b, ruleCombo)
        }
      }
    }
  }



  def simplifyUsingEquivRules2(expr_ : Expr, ruleCombos : Stream[Expr => Option[Expr]]) : (Expr, Int) = {


    val always = (_ : Int) => true

    def perf(expr: Expr, steps : Int) : (Expr, Int) = {
      //add presimplification for performance
      //println(s"pre simple")
      val (out,newSteps) = simplify(expr, always, steps)

      if(newSteps > steps){
        //println(s"first go")
        perf(out, newSteps)
      }else{

        val allPosibilities = applyRules(out, ruleCombos)
        //println("num of possibilities: " + allPosibilities.size)
        //println("num of combos: " + ruleCombos.size)


        for(possibility <- allPosibilities){
          //println(s"posibility: ${possibility.show}")
          val simpl = simplify(possibility, always, newSteps)

          if(simpl._2 > newSteps) return perf(simpl._1, simpl._2)
        }

        (out, newSteps)
      }

    }

    //println("in")
    val res = perf(expr_, 0)
    //println("done")
    res


  }

  def simplifyUsingEquivRules2Stages(expr_ : Expr, ruleCombos : Stream[Expr => Option[Expr]]) : List[Expr] = {


    val oneStep = (i : Int) => i < 1

    def perf(expr: Expr) : List[Expr] = {
      val allPosibilities = applyRules(expr, ruleCombos)

      for(possibility <- allPosibilities){
        val simpl = simplify(possibility, oneStep)

        if(simpl._2 > 0) return simpl._1 :: perf(simpl._1)
      }

      Nil

    }

    //println("in")
    val res = perf(expr_)
    //println("done")
    res


  }



  def show[T](t : T)(implicit showable : Show[T]) = showable.show(t)
  val putStrLn = (t : String) => println(t)



  def substitute(fullExpr: Expr, sym : EVar, sub : Expr) : Expr = {
    fullExpr match{
      case EVar(x) if x == sym.name => sub
      case e@(EInt(_) | EConst(_) | EVar(_) | EInf) => e
      case EUnFn(e, op) => EUnFn(substitute(e, sym, sub), op)
      case EBinFn(e1, e2, op) => EBinFn(substitute(e1, sym, sub), substitute(e2, sym, sub), op)
    }
  }

  val rules = Stream(Stream(useAssocL _, useAssocR _), Stream(useCommut _))
  val combos = makeCombosOfRules(rules)


  implicit class ExprParser(val sc: StringContext){

    object e{

      /*private def deconstructTree(names : List[String], m: Expr, original : Expr, result : List[Expr]) : Option[List[Expr]] = {
        (m, original) match{
          case (EVar(x), expr) if names.contains(x) => Some(expr :: result) //found
          case (EInt(_), _) => None
          case (EUnFn(x1, _), EUnFn(x2,_)) => deconstructTree(names, x1, x2, result)
          case (EBinFn(x1,y1,_), EBinFn(x2,y2,_)) =>
            val t1 = deconstructTree(names, x1, x2, result)
            t1 match{
              case Some(t) =>
                val t2 = deconstructTree(names, y1, y2, result)
                t2 match {
                  case Some(k) => Some(t ++ k)
                  case None => Some(t)
                }
              case None => deconstructTree(names, y1, y2, result)
            }
          case _ => None
        }
      }*/

      //TODO needs more testing
      private def deconstructTreeFullMatch(names : List[String], m: Expr, original : Expr, result : List[Expr]) : Option[List[Expr]] = {
        (m, original) match{
          case (EVar(x), expr) if names.contains(x) => Some(expr :: result) //found
          case (EInt(x), _) => None
          case (EInf, _) => None
          case (EUnFn(x1, ty1), EUnFn(x2,ty2)) if ty1 == ty2 => deconstructTreeFullMatch(names, x1, x2, result)
          case (EBinFn(x1,y1,ty1), EBinFn(x2,y2,ty2)) if ty1 == ty2 =>
            val t1 = deconstructTreeFullMatch(names, x1, x2, result)
            t1 match{
              case Some(t) =>
                val t2 = deconstructTreeFullMatch(names, y1, y2, result)
                t2 match {
                  case Some(k) => Some(t ++ k)
                  case None => Some(t)
                }
              case None => deconstructTreeFullMatch(names, y1, y2, result)
            }
          case _ => None
        }
      }

      def apply(args : Any*) : Expr = {
        val mapped = args.map {
          case expr: Expr => expr.show
          case str : String => str
          case _ => throw new Exception("unsupported interpolation type")
        }
        parse(sc.s(mapped : _*)).get
      }

      def unapplySeq(expr : Expr): Option[Seq[Expr]] = {
        val n = sc.parts.length
        if(n > 1){
          val unknowns = for(i <- 0 until n - 1) yield "$x" + i //TODO add special expr type for deconstruction ?
          val toParse = parse(sc.s(unknowns : _*)) //TODO other way around ? decompose original expr right away ?
          toParse match{
            case Success(x, _) => deconstructTreeFullMatch(unknowns.toList, x, expr, Nil)
            case _ => None
          }
        }else{
          val parsed = parse(sc.s())
          parsed match{
            case Success(p, _) => if(p == expr) Some(Nil) else None
            case _ => None
          }
        }
      }
    }


  }


  case class Vector(val array : Array[Expr]){
    def apply(i : Int) : Expr = {
      array(i)
    }

    def update(i : Int, expr: Expr): Unit = {
      array(i) = expr
    }

    def simplifyAll() : Vector = {
      val ret = array.clone()
      for( i <- 0 until array.length){
        ret(i) = simplifyUsingEquivRules2(this(i), combos)._1
      }

      Vector(ret)
    }

    def isZero(criterion : Expr => Bool) : Bool = array.forall(criterion)
  }

  //each op returns a new matrix
  case class Matrix(val n : Int, val m : Int, val array : Array[Expr]){
    def apply(i : Int, j : Int) : Expr = {
      array(i * m + j)
    }

    def update(i : Int, j : Int, expr: Expr): Unit = {
      array(i * m + j) = expr
    }

    def row(i : Int) : Vector = {
      val ar = new Array[Expr](m)
      for(j <- 0 until m){
        ar(j) = this(i, j)
      }

      Vector(ar)
    }

    def withRow(i : Int, row : Vector) : Matrix = {
      val copy = this.copy()
      for(j <- 0 until m){
        copy(i, j) = row(j)
      }

      copy
    }

    def setRow(i : Int, row : Vector) : Unit  = {
      for(j <- 0 until m){
        this(i, j) = row(j)
      }
    }

    def simplifyAll() : Matrix = {
      val ret = Matrix.newZeroMatrix(n, m)
      for( i <- 0 until n ; j <- 0 until m ){
        ret(i, j) = simplifyUsingEquivRules2(this(i, j), combos)._1
      }

      ret
    }

    def appendColumn(vec : Vector) : Option[Matrix] = {
      if(vec.array.length != this.n) return None

      val ret = Matrix.newZeroMatrix(n, m + 1)
      for( i <- 0 until n ; j <- 0 until m ){
        ret(i, j) = this(i,j)
      }

      for(i <- 0 until vec.array.length){
        ret(i, m) = vec(i)
      }

      Some(ret)
    }

  }



  object Matrix{
    def newZeroMatrix(n : Int, m : Int) : Matrix = {
      val ar = new Array[Expr](n * m)
      for(i <- 0 until ar.length){
        ar(i) = EInt(0)
      }

      Matrix(n, m, ar)
    }

  }

  implicit def showMatrix[M <: Matrix]: Show[M] = {
    mat =>
      var str = s"Matrix(${mat.n} x ${mat.m})\n"
      for(i <- 0 until mat.n){
        for(j <- 0 until mat.m){
          str += mat(i,j).show + " "
        }
        str += "\n"
      }

      str
  }

  implicit def showVector[V <: Vector]: Show[V] = {
    vec =>
      var str = s"Vector(${vec.array.length})\n"
      for(i <- vec.array.indices){
        str += vec(i).show + " "
      }

      str += "\n"

      str
  }

  def swapRows(mat : Matrix, i : Int, j : Int) : Matrix = {
    val rowI = mat.row(i)
    val rowJ = mat.row(j)
    val n = mat.withRow(i, rowJ)
    n.withRow(j, rowI)
  }


  def swapIfNeeded(mat : Matrix, i : Int, isZero : Expr => Bool) : Option[Matrix] = {

    if(!isZero(mat(i,i))) return Some(mat)

    for(k <- i + 1 until mat.n){
      if(!isZero(mat(k,i))){
        return Some(swapRows(mat, i, k))
      }
    }

    None
  }


  //TODO needs extensive testing
  def areCollinear(a : Vector, b : Vector, isZero : Expr => Bool, simplify : Expr => Expr) : Bool = {
    if(a.array.length != b.array.length || a.array.length == 0) return false


    var j = -1

    var k : Expr = EInt(0)

    Breaks.breakable{ //find first non zero
      for(i <- a.array.indices){
        val bi = b(i)
        if(!isZero(bi)){
          k = simplify(EBinFn(a(i),bi,Div))
          j = i
          Breaks.break()
        }
      }
    }


    if(j == -1){ //b is zero
      return a.isZero(isZero)
    }

    for(i <- j + 1 until a.array.length){
      val k2 = simplify(EBinFn(a(i),b(i),Div))

      if(k != k2 && !(isZero(a(i)) && isZero(b(i)) )) return false
    }

    true

  }

  def checkCollinearRows(mat_ : Matrix, isZero : Expr => Bool, simplify : Expr => Expr) : Matrix = {

    val mat = mat_.copy()

    for(i <- 0 until mat.n - 1){
      for(j <- i + 1 until mat.n){
        println("are col: " + mat.row(i).show + " and " + mat.row(j).show)
        if(areCollinear(mat.row(i), mat.row(j), isZero, simplify)){
          mat.setRow(j, Vector(Array.tabulate(mat.n)(_ => EInt(0)))) //set to zero vector
        }
      }
    }

    mat
  }

  def allZerosToBottom(mat_ : Matrix, isZero : Expr => Bool) : Matrix  = {

    var mat = mat_.copy()

    for(i <- 0 until mat.n - 1){
      val v = mat.row(i)

      if(v.isZero(isZero)){
        Breaks.breakable{
          for(j <- i + 1 until mat.n){
            val non = mat.row(j)
            if(non.isZero(isZero)){
              mat = swapRows(mat, i, j)
              Breaks.break()
            }
          }
        }
      }
    }

    mat
  }

  //TODO needs extensive testing
  def matrixToRowEchelonForm(mat : Matrix, isZero : Expr => Bool, simplify : Expr => Expr, i : Int = 0): Matrix ={

    if(i + 1 == mat.m){
      val mat1 = checkCollinearRows(mat, isZero, simplify)
      val mat2 = allZerosToBottom(mat1, isZero)
      return mat2
    }

    val swap = swapIfNeeded(mat, i, isZero) //finds first row with non zero expr
    swap match{
      case Some(mat) =>
        println(s"swapped: \n" + mat.show)
        val onDiagonal = mat(i, i)
        val one = EInt(1)

        val newMat = mat.copy()

        newMat(i, i) = one

        for(j <- i + 1 until mat.m){
          newMat(i, j) = EBinFn(mat(i, j), onDiagonal, Div)
        }

        for(k <- i + 1 until mat.n){
          val el = newMat(k, i)//will become 0
          println("el: " + el.show)
          newMat(k,i) = EInt(0)
          for(j <- i + 1 until mat.m){
            newMat(k, j) = EBinFn(newMat(k,j), EBinFn(EBinFn(EInt(-1), newMat(i, j), Mult), el, Mult), Plus) //mat(k, j) - mat(i, j) * el
            if(k == 2) println(s"(3,${j+1}) = ${newMat(k,j).show}")
          }
        }

        println(s"step $i ${newMat.simplifyAll().show}")

        matrixToRowEchelonForm(newMat.simplifyAll(), isZero, simplify, i + 1)
      case None => matrixToRowEchelonForm(mat.simplifyAll(), isZero, simplify, i + 1)
    }


  }


  def countLeadingZeros(vec : Vector, isZero : Expr => Bool) : Int = {
    var count = 0
    for(a <- vec.array if isZero(a)) count += 1
    count
  }

  def isInRowEchelonForm(mat : Matrix, isZero : Expr => Bool) : Bool = {

    var prev = -1

    for(row <- 0 until mat.n){
      val count = countLeadingZeros(mat.row(row), isZero)
      if(count > prev){
        prev = count
      }else{
        return false
      }
    }

    true
  }

  //TODO needs extensive testing
  def matrixRank(mat : Matrix, isZero : Expr => Bool, simplify : Expr => Expr) : Int = {
    val rowEchelon = if(isInRowEchelonForm(mat, isZero)) mat else matrixToRowEchelonForm(mat, isZero, simplify)

    var rank = rowEchelon.n
    for(i <- rowEchelon.n - 1 to 0 by -1){
      if(rowEchelon.row(i).isZero(isZero)) rank -= 1
    }

    rank
  }

  def solveLinearSystemSingular(mat : Matrix, vec : Vector, isZero : Expr => Bool, simplify : Expr => Expr) : Vector = {
    println(s"input mat ${mat.appendColumn(vec).get.simplifyAll().show}")
    val tr = matrixToRowEchelonForm(mat.appendColumn(vec).get, isZero, simplify)
    println(s"found matrix: ${tr.simplifyAll().show}")
    val ar = new Array[Expr](vec.array.length)

    for(i <- ar.length - 1 to 0 by -1){
      val ii = tr(i,i)

      val last = tr(i, tr.m - 1)

      var expr = last

      for(j <- tr.m - 2 until i by -1){
        expr = EBinFn(expr, EBinFn(EBinFn(ar(j), tr(i,j), Mult), EInt(-1), Mult), Plus)
      }

      ar(i) = EBinFn(expr, ii, Div)

    }


    val res = Vector(ar)

    res
  }


  /*def perfectSquare(expr: Expr) : Option[Expr] = {
    expr match{
      case EInt(x) if isPerfectSquare(x) => Some(EUnFn(EInt(math.sqrt(x).toInt), Square))
      case EBinFn(a, b, Mult) if a == b => Some(EUnFn(a, Square))
      case EBinFn(EBinFn(EUnFn(a, Square), EBinFn(EInt(2), EBinFn(b,c,Mult), Mult), Plus), EUnFn(d, Square), Plus)
        if a == b && c == d => Some(EUnFn(EBinFn(a,d, Plus), Square))
      case EBinFn(EBinFn(EUnFn(a, Square), EBinFn(EInt(2), EBinFn(b,c,Mult), Mult), Minus), EUnFn(d, Square), Plus)
        if a == b && c == d => Some(EUnFn(EBinFn(a,d, Minus), Square))
      case _ => None
    }
  }*/
}