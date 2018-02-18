package me.russoul.mather

import Mather._

import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

object NewParser extends RegexParsers{
  override val skipWhitespace = true

  val space = "[ ]"
  val letter = "[a-zA-Z]"
  val digit = "[0-9]"
  val sym = "[\\+\\-\\*/]"

  def parseInt: Parser[EInt] = {
    opt("-") ~ rep1(digit.r) ^^ { case (minus ~ str) => EInt(Integer.parseInt( (if(minus.isDefined) "-" else "") + str.reduce(_ + _))) }
  }

  def parseConst: Parser[Expr] = {
    (opt("-") ~ regex(letter.r) ~ rep(letter.r | digit.r)) ^? {
      case (minus ~ x ~ xs) if !unFnSyms.values.exists(v => v == (x :: xs).reduce(_ + _)) && !binOpSyms.values.exists(v => v == (x :: xs).reduce(_ + _)) && !binFnSyms.values.exists(v => v == (x :: xs).reduce(_ + _)) && constants.contains((x :: xs).reduce(_ + _)) =>
        if(minus.isDefined){
          EBinFn(EInt(-1), EConst((x :: xs).reduce(_ + _)), Mult)
        }else{
          EConst((x :: xs).reduce(_ + _))
        }
    }
  }

  def parseVar: Parser[Expr] = {
    (opt("-") ~ opt(literal("$")) ~ regex(letter.r) ~ rep(letter.r | digit.r)) ^? {
      case (minus ~ dollar ~ x ~ xs) if !unFnSyms.values.exists(v => v == (x :: xs).reduce(_ + _)) && !binOpSyms.values.exists(v => v == (x :: xs).reduce(_ + _)) && !binFnSyms.values.exists(v => v == (x :: xs).reduce(_ + _)) && !constants.contains((x :: xs).reduce(_ + _)) =>
        val res = EVar((if(dollar.isDefined) "$" else "") + (x :: xs).reduce(_ + _))
        if(minus.isDefined){
          EBinFn(EInt(-1), res, Mult)
        }else{
          res
        }

    }
  }

  //TODO code duplication
  def parseVarWithoutMinus: Parser[EVar] = {
    (regex(letter.r) ~ rep(letter.r | digit.r)) ^? {
      case (x ~ xs) if !unFnSyms.values.exists(v => v == (x :: xs).reduce(_ + _)) && !binOpSyms.values.exists(v => v == (x :: xs).reduce(_ + _)) && !binFnSyms.values.exists(v => v == (x :: xs).reduce(_ + _)) && !constants.contains((x :: xs).reduce(_ + _)) =>
        EVar((x :: xs).reduce(_ + _))

    }
  }

  def parseUnFnSimple : Parser[UnFn] = {
    rep1(letter.r) ^? {
      case xs if unFnSyms.values.exists(x => x == xs.reduce(_ + _)) =>
        keyForValue(unFnSyms, xs.reduce(_ + _)).get
    }
  }



  def parseUnFnDefaultNotation : Parser[Expr] = {

    (opt("-") ~ parseUnFnSimple <~ literal("(")) ~ (parseExpr <~ literal(")")) ^^ {
      case minus ~ typee ~ content =>


        val res = typee match{
          case Sqrt => EBinFn(content, EBinFn(EInt(1), EInt(2), Div), Pow)
          case _ => EUnFn(content, typee)
        }

        if(minus.isDefined){
          EBinFn(EInt(-1), res, Mult)
        }else{
          res
        }
    }
  }


  def parseDifOp : Parser[Expr] = {
    opt("-") ~ (literal("d/d") ~> parseVarWithoutMinus <~ literal("(")) ~ (parseExpr <~ literal(")")) ^^ {
      case minus ~ variable ~ expr => if(minus.isDefined) EBinFn(EInt(-1), EUnFn(expr, Dif(variable)), Mult) else EUnFn(expr, Dif(variable))
    }
  }

  def parsePow : Parser[Expr] = {
    (opt("-") <~ literal("pow")) ~ (literal("(") ~> ((parseExpr <~ literal(",")) ~ parseExpr) <~ literal(")")) ^^ {
      case minus ~ (expr1 ~ expr2) =>
        if(minus.isDefined){
          EBinFn(EInt(-1), EBinFn(expr1, expr2, Pow), Mult)
        }else{
          EBinFn(expr1, expr2, Pow)
        }
    }
  }


  case class PosString(str : String) extends Positional

  def parsePosSym : Parser[PosString] = {
    regex(sym.r) ^^ {x => PosString(x)}
  }

  //op must be available in `binOpSyms`
  //TODO forbid this kind of situation: 1 + -1, allow only: 1 + (-1)
  def parseLeftAssocOpsOnSameLevel : Parser[EBinFn] = {
    parseOpExpr ~ rep1(positioned(parsePosSym) ~ parseOpExpr) ^^ {
      case x ~ ( (op ~ expr) :: xs) =>
       def rec(left : Expr, op : PosString, right : Expr,  xs : List[~[PosString,Expr]]) : EBinFn = {
         //third num is the index of operator with least precedence
         val leastPrecedence = xs.foldLeft((op, keyForValue(binOpSyms, op.str).get, 0, 0)){ case (ac, op ~ _) =>
           val opK = keyForValue(binOpSyms, op.str).get
           if(precedence(ac._2) >= precedence(opK)) (op,opK,ac._4 + 1, ac._4 + 1) else (ac._1,ac._2,ac._3,ac._4+1)}.asInstanceOf[(Positional, BinFn,Int,Int)]




         if(xs.isEmpty){
           EBinFn(left, right, leastPrecedence._2)
         }else{
           if(leastPrecedence._3 == 0){
             EBinFn(left, rec(right, xs.head._1, xs.head._2, xs.tail), leastPrecedence._2)
           }else if (leastPrecedence._1 == xs.last._1.asInstanceOf[Positional]){
             EBinFn(rec(left, op, right, xs.init), xs.last._2, leastPrecedence._2)
           }else{
             val split = xs.splitAt(leastPrecedence._3)
             val leftPart = rec(left, op, right, split._1.init)
             /*println("left part:" + split._1)
             println("right part: " + split._2)
             println("spliter: " + leastPrecedence._2)
             println("left:" + left)
             println("right: " + right)
             println("op: " + op)
             println("full list: " + xs)*/
             val rightPart = rec(split._1.last._2, split._2.head._1, split._2.head._2, split._2.tail)

             EBinFn(leftPart, rightPart, leastPrecedence._2)
           }
         }

       }


       rec(x, op, expr, xs)


    }
  }


  def parseOpExprNoP : Parser[Expr] = {
      parseInt |
      parseDifOp | //must go before var
      parsePow |
      parseConst |
      parseVar |
      parseUnFnDefaultNotation
  }

  def parseOpExpr : Parser[Expr] = parseOpExprNoP | parseExprP

  def parseExprP : Parser[Expr] = {
    literal("(") ~ ( parseLeftAssocOpsOnSameLevel |
                              parseInt                 |
                              parseDifOp | //must go before var
                              parsePow |
                              parseConst               |
                              parseVar                 |
                          parseUnFnDefaultNotation) ~ literal(")") ^^ {
      case _ ~ expr ~ _ => expr
    }
  }

  def parseExprNoP : Parser[Expr] = {
    parseLeftAssocOpsOnSameLevel | //must always(or almost always) be first
      parseInt |
      parseDifOp | //must go before var
      parsePow |
      parseConst |
      parseVar |
      parseUnFnDefaultNotation
  }

  def parseExpr : Parser[Expr] = parseExprNoP | parseExprP



  def parse(str : String) : ParseResult[Expr] = {
    parseAll(parseExpr, str)
  }

}

object Parser {


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

  //substring must be equal to sum defined bin fn symbol
  //full string must contain no empty lhs and rhs relative to substring
  def parseBinFn(substring : String, fullstring : String) : Option[BinFn] = {
    for(sym <- binOpSyms){
      if(sym._2 == substring){
        if(fullstring.matches(s"[\\s\\S]*\\S+[\\s\\S]*\\$substring[\\s\\S]*\\S+[\\s\\S]*")) return Some(sym._1)
      }
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

    val pattern = (s"dif_([^1-9][1-9a-zA-Z]*)\\s*\\((.*)\\)").r
    val result = pattern.findFirstMatchIn(str).map(x => (x.group(1), x.group(2)) )
    val fil = result.filter{ case (variable, insides) => isValidEVar(variable) && areParenthesesValid(insides) }
    if(fil.isDefined) return Some( (Dif(EVar(fil.get._1)), fil.get._2) )


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

  def stringHasOnlyDigits(str : String) : Bool = {
    for(char <- str){
      if(char < '0' || char > '9') return false
    }

    str.nonEmpty
  }


  def stringStartsWithDigit(str : String) : Bool = str.nonEmpty && str.head >= '0' && str.head <= '9'

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

  def stringOfLettersDigitsSpecial(str : String, special : List[Char]) : Bool = {
    for(char <- str){
      if(!char.isLetterOrDigit && !special.contains(char)) return false
    }

    true
  }

  //accepts trimmed input
  def isValidEVar(str : String) : Bool = {
    !stringStartsWithDigit(str) && str.nonEmpty && !stringHasWhitespace(str) && stringOfLettersDigitsSpecial(str, List('$'))//$ is used internally
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
          val maybeBinFn = parseBinFn(tryStr, str)
          maybeBinFn match{
            case Some(f) => if(findSymLevel(str, i) == 0) buf.append((f, i, j))
            case None => ()

          }
        }
      }

      if(buf.nonEmpty){
        //TODO debug
        //buf.foreach(putStrLn compose show)

        var chosen = buf.head
        for(f <- buf){
          if(precedence(f._1) <= precedence(chosen._1)){ //choose the last one with least precedence
            chosen = f
          }
        }

        //println("chosen " + chosen)


        //set correct associativity if multiple associative bin fns are at the same level:
        //x + y + z => (x + y) + z
        for(f <- buf){
          if(f._1 == chosen._1){
            getAssoc(chosen._1) match{
              case AssocLeft => if(f._2 > chosen._2) chosen = f //choose the rightmost first
              case AssocRight => if(f._2 < chosen._2) chosen = f //choose the leftmost first
            }
          }
        }

        if(chosen._2 == 0 || chosen._3 == str.length) return None //symbol (operator) of bin starts from the first char of the string or from the last one

        val lhs = str.substring(0, chosen._2)
        val rhs = str.substring(chosen._3, str.length)

        //TODO debug
        //println(s"got string $str")

        //TODO debug
        //println(s"op ${chosen}")

        //println(s"lhs ${lhs}")
        //println(s"rhs ${rhs}")


        val lhsMade = make(lhs)
        val rhsMade = make(rhs)

        //println(s"lhsm ${lhsMade} ${lhs}")
        //println(s"rhsm ${rhsMade} ${rhs}")

        for(lhs <- lhsMade; rhs <- rhsMade) yield EBinFn(lhs, rhs, chosen._1)

        //...
      }else{ //x or 12345
        val trimmed = str.trim
        //TODO debug
        //println(trimmed)

        val maybeUnFn = parseUnFn(trimmed)
        maybeUnFn match{
          case Some(f) =>
            for(arg <- make(f._2)) yield EUnFn(arg, f._1)
          case None =>
            if(isValidEInt(trimmed)){
              //TODO debug
              //println("EInt:" + trimmed)
              Some(EInt(Integer.parseInt(trimmed)))
            }
            else if(isValidEVar(trimmed)){//if EVar is valid then EConst is also valid (same rules)
              if(constants.contains(trimmed)){
                Some(EConst(trimmed))
              }else{
                Some(EVar(trimmed))
              }
            }else
              None
        }
      }
    }


    make(str_)

  }


}
