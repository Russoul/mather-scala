import cats._
import cats.data._
import cats.implicits._
//import cats.syntax._

import scala.reflect.ClassTag
import scala.collection.immutable
import scala.collection.mutable

object Mather {

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
  val unFnSyms = immutable.HashMap(Sqrt -> "sqrt", Square -> "square", Module -> "abs")

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
    if(a < 0) return false

    val sqrt = math.sqrt(a)
    val floor = math.floor(sqrt)
    floor * floor == a.toDouble
  }

  def GCD(a: Int, b: Int): Int = {
    if (b == 0) return a
    GCD(b, a % b)
  }


  /*sealed trait Ty
  case object TyInt extends Ty
  case class TyBinFn[A <: Ty, B <: Ty, F <: BinFn](a : A, b : B, f : F) extends Ty
  case object TyVar extends Ty
  case class TyUnFn[A <: Ty, F <: UnFn](a : A, f : F) extends Ty*/

  //TODO VERY IMPORTANT NOTICE !!!!!!!
  //In order to simplify things A LOT + make them mathematically (theoretically) correct
  //we assume that every expression IS a FUNCTION
  //i.e
  //EInt(1) is a function of unknown number of arguments that returns a constant value `1` at each point of its domain
  //EConst(name) (to be added) is a function of unknown number of arguments that returns some unknown constant value (or parameter if you will) at each point of its domain
  //EVar(name) is a function of one argument - `name`, which is equal to its variable at each point of its domain
  //notice about domains:
  //we should somehow create a notation to express a domain of a function
  //EUnFn and EBinFn are complex functions (functions of functions) TODO write more about complex functions
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
          case Div => if (isWholeDivision(x, y) && (y != 0)) EInt(x / y) else {

            def simpl(x : Int, y : Int) : (Int, Int) = {
              val gcd = GCD(x,y)
              if(gcd == 1) return (x, y)

              simpl(x / gcd, y / gcd)
            }

            val gcdProc = simpl(x, y)

            if(gcdProc._1 < 0 && gcdProc._2 < 0) EBinFn(EInt(-gcdProc._1), EInt(-gcdProc._2), Div) else EBinFn(EInt(gcdProc._1), EInt(gcdProc._2), Div)

          }
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
      /*case EBinFn(EBinFn(EInt(k1),EVar(v1), Mult), EVar(v2), Div) if v1 == v2 =>
        //(k1 * x) / x) //should not do this as x may be 0
        EInt(k1)*/
      case EBinFn(EBinFn(EInt(a), EInt(b), Div), EBinFn(EInt(c), EInt(d), Div), Mult) =>
        //(a/b) * (c/d)
        EBinFn(EInt(a * c), EInt(b * d), Div)
      case EBinFn(a, EBinFn(c, EInt(d), Div), Plus) if d != 0 => //a + c/d //where d is a number != 0
        EBinFn(EBinFn(EBinFn(a, EInt(d), Mult), c, Plus), EInt(d), Div)
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
      case i@EInt(_) => i #:: ruleCombos.flatMap(rule => rule(i))
      case v@EVar(_) => v #:: ruleCombos.flatMap(rule => rule(v))
      case f@EUnFn(arg, op) =>
        val inner = applyRules(arg, ruleCombos)
        f #:: inner.flatMap(
          a => ruleCombos.flatMap(
            rule => rule(EUnFn(a, op))))

      case f@EBinFn(a, b, op) =>
        val inner1 = applyRules(a, ruleCombos)
        val inner2 = applyRules(b, ruleCombos)
        f #:: inner1.flatMap(
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
      case i@EInt(_) => ruleCombo(i)
      case v@EVar(_) => ruleCombo(v)
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

  //trimmed input
  //what it does: "(((*)))" => "*"
  def removeRedundantParentheses(str : String) : String = {
    if(str.length < 2) return str

    if(str(0) == '(' && str.last == ')'){

      var counter = 0
      for(char <- str.substring(1, str.length - 1)){ //make sure that in this case: (3 + x) + ((x * 2) / x) first and last parentheses are not removed
        if(char == '('){
          counter += 1
        }else if(char == ')'){
          counter -= 1
        }

        if(counter < 0) return str
      }

      removeRedundantParentheses(str.substring(1, str.length - 1))
    }
    else str
  }

  //the amout of opening brackets must be equal to the amout of closing brackets
  def areParenthesesValid(str : String) : Bool = {
    var counter = 0
    for(char <- str){
      if(char == ')') counter += 1
      else if(char == '(') counter -= 1
    }

    counter == 0
  }

  def parseBinFn(str : String) : Option[BinFn] = {
    for(sym <- binFnSyms){
      if(sym._2 == str) return Some(sym._1)
    }
    None
  }

  //example input: sqrt(2 + 5 * pow(1,2)) -> result should be Some(sqrt)
  def parseUnFn(str : String) : Option[(UnFn,String)] = {
    for((value,sym) <- unFnSyms){
      val pattern = (s"^$sym\\s*\\((.*)\\)" + "$").r
      val result = pattern.findFirstMatchIn(str).map(_.group(1))
      val fil = result filter areParenthesesValid //validity of parentheses of the argument to $sym is checked here
      val res = fil.map(x => (value, x) )

      if(res.isDefined) return res
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
      stringHasOnlyDigits(str) && str.head != '0' || str == "0"
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
      val str = {val t = str_.trim; removeRedundantParentheses(t)}
      //println(s"got $str")


      val buf = new mutable.ArrayBuffer[(BinFn, Int, Int)]()
      //every unary function has its precedence above any binary function //TODO for now ?

      for(i <- 0 until str.length){
        for(j <- i to str.length){ //to here because substring endIndex may be equal to length
          val tryStr = str.substring(i, j)
          val maybeBinFn = parseBinFn(tryStr)
          maybeBinFn match{
            case Some(f) => if(findSymLevel(str, i) == 0) buf.append((f, i, j))
            case None => ()

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

        val maybeUnFn = parseUnFn(trimmed)
        maybeUnFn match{
          case Some(f) =>
            for(arg <- make(f._2)) yield EUnFn(arg, f._1)
          case None =>
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
    }


    make(str_)

  }

  val rules = Stream(Stream(useAssocL _, useAssocR _), Stream(useCommut _))
  val combos = makeCombosOfRules(rules)


  implicit class ExprParser(val sc: StringContext) extends AnyVal {
    def e(args : Any*) : Expr = parse(sc.parts.head).get

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
  }

  //each op returns a new matrix
  case class Matrix(val n : Int, val m : Int, val array : Array[Expr]){
    def apply(i : Int, j : Int) : Expr = {
      array(i * m + j)
    }

    def update(i : Int, j : Int, expr: Expr): Unit = {
      array(i * m + j) = expr
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


  def matrixToHigherTriangularFormNoZeroChecks(mat : Matrix, i : Int = 0): Matrix ={

    if(i + 1 == mat.m) return mat

    val onDiagonal = mat(i, i)
    val one = EInt(1)

    val newMat = mat.copy()

    newMat(i, i) = one

    for(j <- i + 1 until mat.m){
      newMat(i, j) = EBinFn(mat(i, j), onDiagonal, Div)
    }

    for(k <- i + 1 until mat.n){
      val el = newMat(k, i)//will become 0
      newMat(k,i) = EInt(0)
      for(j <- i + 1 until mat.m){
        newMat(k, j) = EBinFn(newMat(k,j), EBinFn(EBinFn(EInt(-1), newMat(i, j), Mult), el, Mult), Plus) //mat(k, j) - mat(i, j) * el
      }
    }

    println(s"step $i ${newMat.simplifyAll().show}")

    matrixToHigherTriangularFormNoZeroChecks(newMat.simplifyAll(), i + 1)
  }

  def solveLinearSystemSingular(mat : Matrix, vec : Vector) : Vector = {
    println(s"input mat ${mat.appendColumn(vec).get.simplifyAll().show}")
    val tr = matrixToHigherTriangularFormNoZeroChecks(mat.appendColumn(vec).get)
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