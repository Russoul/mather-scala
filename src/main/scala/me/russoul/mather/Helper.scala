package me.russoul.mather

import scala.collection.immutable.HashMap
import scala.collection.mutable

object Helper {



  import Mather._

  import Interval._
  val NegEInf = EBinFn(EInt(-1), EInf, Mult)


  //e.g: e"x + y + z + sin(x) + cos(pow(2 * t))" has bindings: {x,y,z,t}
  def findBindings(expr : Expr) : Set[EVar] = {
    expr match{
      case (EInt(_) | EConst(_) | EInf) => Set.empty
      case b@EVar(_) => Set(b)
      case EUnFn(a, _) => findBindings(a)
      case EBinFn(a, b, _) => findBindings(a) ++ findBindings(b)
    }
  }

  //checks whether the variable is dependent or free(independent)
  def isFree(variable : EVar, env : FuncEnv) : Boolean = {
    !env.env.contains(variable)
  }

  def getFuncDef(name : EVar, scope : Scope) : Option[FuncDef] = {
    for(l <- scope.defs){
      if(l.binding == name) return Some(l)
    }

    None
  }


  def makeScope(exprs : List[Expr]) : Option[Scope] = {

    def processFuncDef(funcDef : FuncDef, env : FuncEnv) : Boolean = {
      val funcName = funcDef.binding
      if(env.env.contains(funcName))
        false
      else{
        funcDef.expr match{
          case Right(bindings) =>
            env.env += (funcName -> bindings)
            true
          case Left(expr) =>
            env.env += (funcName -> findBindings(expr).toList)
            true
        }
      }


    }

    val defs = exprs.init
    val last = exprs.last

    val env = FuncEnv(new mutable.HashMap())

    val ok =
      for(def_ <- defs) yield{
        if(!def_.isInstanceOf[FuncDef])
          false
        else
          processFuncDef(def_.asInstanceOf[FuncDef], env)
      }

    if(ok.isEmpty || ok.forall(x => x))
      Some(Scope(defs.asInstanceOf[List[FuncDef]], last, env))
    else
      None
  }




  //domain of expr TODO
  //example


  //sin(y)/z

  //we consider y and z as independent variables even if the scope contains them as dependent

  //domain of the above expr is {y -> R, z -> R\{0}}


  //image of expr TODO



  //the following is not correct and not usable VVV

/*
  def findDomain(scope : Scope) : Option[HashMap[EVar, IInterval]] = { //TODO functions of complex arguments
    val bindings = findBindings(scope.expr)
    val doms = new mutable.HashMap[EVar, IInterval]

    for(binding <- bindings.toList) {
      if(isFree(binding, scope.env)) doms += (binding -> IR)
      else{
        val funcDef = getFuncDef(binding, scope).get
        if(funcDef.expr.isLeft){
          findDomain(Scope(scope.defs, funcDef.expr.left.get, scope.env)) match{
            case Some(x) => doms += (binding -> x)
            case None => return None
          }
        }else{
          return None
        }
      }

    }


    def domainOfExpr(expr : Expr, env : mutable.HashMap[EVar, IInterval]) : Option[IInterval] = {
      expr match{
        case (EInt(_) | EConst(_) | EInf) =>
        case b@EVar(_) =>
        case EUnFn(a, _) =>
        case EBinFn(a, b, _) =>
      }
    }

    //TODO



  }*/


  //TODO automatic solvers of equation: f(x) = 0; (given expr `a` solver would find the type of equation and solve it if type is known)

  //ax + b = 0
  def solveLinearEq(a : Expr, b : Expr) : Expr = {
    e"-1 * $b / (2 * $a)"
  }

  //ax^2 + bx + c = 0
  def solveQuadraticEq(a : Expr, b : Expr, c : Expr) : List[Expr] = {
    val d = e"$b * $b - 4 * $a * $c"
    List(e"(-1 * $b + sqrt($d))/(2 * $a)", e"(-1 * $b - sqrt($d))/(2 * $a)")
  }

}
