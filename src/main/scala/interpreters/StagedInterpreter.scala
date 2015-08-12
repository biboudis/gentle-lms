package interpreters

import scala.reflect.Manifest
import scala.Array
import scala.language.higherKinds
import scala.virtualization.lms.common._

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

trait StagedInterpreter extends IfThenElse
    with BooleanOps with Variables with OrderingOps with PrimitiveOps
    with LiftVariables with LiftBoolean with LiftPrimitives with ArrayOps
    with ArrayBufferOps with While with NumericOps
    with Equal with LiftString with StringOps {   }

object StagedInterpreter {
  class YikesEx(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

  def env0[A, B] : (A => B) = (x : A) => throw new YikesEx()

  def fenv0[A, B] : (A => B) = env0

  def eval1(e : Exp, env: (String => Int), fenv: (String => (Int  => Int))) : Int = {
    e match {
      case IntVal(i) => i
      case Variable(s) => env(s)
      case App(s, e2) => fenv(s)(eval1(e2, env, fenv))
      case Add(e1, e2) => eval1(e1, env, fenv) + eval1(e2, env, fenv)
      case Div(e1, e2) => eval1(e1, env, fenv) + eval1(e2, env, fenv)
      case Mul(e1, e2) => eval1(e1, env, fenv) + eval1(e2, env, fenv)
      case Sub(e1, e2) => eval1(e1, env, fenv) + eval1(e2, env, fenv)
      case Ifz(e1,e2,e3) =>
        if (eval1(e1, env, fenv)==0)
          eval1(e2, env, fenv)
        else
          eval1(e3, env, fenv)
    }
  }

  // How to simulate higher-rank polymorphism in Scala?
  def extend [A, B](env  : (A => B       ), x : A, v : B       ) = (y : A) => if(x == y) v else env(y)
  def extendf[A, B](fenv : (A => (B => B)), x : A, v : (B => B)) = (y : A) => if(x == y) v else fenv(y)

  def peval1(p: Prog, env : (String => Int), fenv: (String => (Int => Int))) : Int = {
    p match {
      case Program(Nil, e) => eval1(e, env, fenv)
      case Program(Declaration(name, param, e1)::defs, e) => {
        def f(x : Int) : Int = eval1(e1, extend(env, param, x), extendf(fenv, name, f))
        peval1(Program(defs, e), env, extend(fenv, name, f))
      }
    }
  }
}
