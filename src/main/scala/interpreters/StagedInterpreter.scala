package interpreters

import scala.reflect.Manifest
import scala.Array
import scala.language.higherKinds
import scala.lms.common._

// type exp = Int of int | Var of string | App of string * exp
//    | Add of exp * exp | Sub of exp * exp
//    | Mul of exp * exp | Div of exp * exp | Ifz of exp * exp * exp
// type def = Declaration of string * string * exp
// type prog = Program of def list * exp

abstract class Exp
case class IntVal(value : Int) extends Exp
case class Variable(name : String) extends Exp
case class App(id : String, exp : Exp) extends Exp
case class Add(exp1: Exp, exp2: Exp) extends Exp
case class Div(exp1: Exp, exp2: Exp) extends Exp
case class Mul(exp1: Exp, exp2: Exp) extends Exp
case class Sub(exp1: Exp, exp2: Exp) extends Exp
case class Ifz(exp1: Exp, exp2: Exp, exp3: Exp) extends Exp

abstract class Def
case class Declaration(name: String, param: String, body: Exp) extends Def

abstract class Prog
case class Program(defs: List[Declaration], body: Exp) extends Prog

class YikesEx(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

object BasicInterpreter {
  def env0[A, B] : (A => B) = (x : A) => throw new YikesEx()

  def fenv0[A, B] : (A => B) = env0

  def extend [A, B](env : (A => B), x : A, v : B) = (y : A) => if(x == y) v else env(y)

  def eval1(e : Exp, env: (String => Int), fenv: (String => (Int  => Int))) : Int = {
    e match {
      case IntVal(i) => i
      case Variable(s) => env(s)
      case App(s, e2) => fenv(s)(eval1(e2, env, fenv))
      case Add(e1, e2) => eval1(e1, env, fenv) + eval1(e2, env, fenv)
      case Div(e1, e2) => eval1(e1, env, fenv) / eval1(e2, env, fenv)
      case Mul(e1, e2) => eval1(e1, env, fenv) * eval1(e2, env, fenv)
      case Sub(e1, e2) => eval1(e1, env, fenv) - eval1(e2, env, fenv)
      case Ifz(e1, e2, e3) =>
        if (eval1(e1, env, fenv) == 0)
          eval1(e2, env, fenv)
        else
          eval1(e3, env, fenv)
    }
  }

  def peval1(p: Prog, env : (String => Int), fenv: (String => (Int => Int))) : Int = {
    p match {
      case Program(Nil, e) => eval1(e, env, fenv)
      case Program(Declaration(s1, s2, e1)::t1, e) => {
        def f(x : Int) : Int = eval1(e1, extend(env, s2, x), extend(fenv, s1, f))
        peval1(Program(t1, e), env, extend(fenv, s1, f))
      }
    }
  }
}

object StagedInterpreter {}
trait StagedInterpreter extends IfThenElse
    with BooleanOps with Variables with OrderingOps with PrimitiveOps
    with LiftVariables with LiftBoolean with LiftPrimitives with ArrayOps
    with ArrayBufferOps with While with NumericOps
    with Equal with LiftString with StringOps with Functions {

  def extend[A, B](env : (A => B), x : A, v : B) = (y : A) => if(x == y) v else env(y)

  // the simple interpreter staged
  def eval2(e : Exp, env: (String => Rep[Int]), fenv: (String => Rep[Int => Int])) : Rep[Int] = {
    e match {
      case IntVal(i) => i
      case Variable(s) => env(s)
      case App(s, e2) => fenv(s)(eval2(e2, env, fenv))
      case Add(e1, e2) => eval2(e1, env, fenv) + eval2(e2, env, fenv)
      case Div(e1, e2) => eval2(e1, env, fenv) / eval2(e2, env, fenv)
      case Mul(e1, e2) => eval2(e1, env, fenv) * eval2(e2, env, fenv)
      case Sub(e1, e2) => eval2(e1, env, fenv) - eval2(e2, env, fenv)
      case Ifz(e1, e2, e3) =>
        if (eval2(e1, env, fenv) == 0)
          eval2(e2, env, fenv)
        else
          eval2(e3, env, fenv)
    }
  }

  def peval2(p: Prog, env : (String => Rep[Int]), fenv: (String => Rep[Int => Int])) : Rep[Int] = {
    p match {
      case Program(Nil, e) => eval2(e, env, fenv)
      case Program(Declaration(s1, s2, e1)::t1, e) => {
        def f: Rep[Int => Int] = fun { x: Rep[Int] =>
          eval2(e1, extend(env, s2, x), extend(fenv, s1, f))
	}
        peval2(Program(t1, e), env, extend(fenv, s1, f))
      }
    }
  }
}

