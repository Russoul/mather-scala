package me.russoul.mather

import Mather._

import scala.collection.mutable

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
    for(sym <- binFnSyms){
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
