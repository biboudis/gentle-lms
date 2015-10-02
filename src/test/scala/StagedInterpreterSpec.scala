import lms._
import interpreters._
import org.scalatest._

import scala.lms.common._
import scala.lms.internal._
import scala.language.reflectiveCalls
import java.io.PrintWriter

class StagedInterpreterSpec extends FlatSpec with Matchers {

  import interpreters.BasicInterpreter._
  import interpreters.StagedInterpreter._

  trait StagedInterpreterTests extends StagedInterpreter {
    val add  = Program(Nil, Add(IntVal(1), IntVal(2)))

    val fact = Program(List(Declaration("fact","x", Ifz(Variable("x"),
      IntVal(1), Mul(Variable("x"), App("fact", Sub(Variable("x"), IntVal(1))))))),
      App ("fact", IntVal(5)))

    def pevalAddTest(test : Rep[Int]) : Rep[Int] = peval2(add, env0, fenv0)
    def pevalFactTest(test : Rep[Int]) : Rep[Int] = peval2(fact, env0, fenv0)
  }

  val staged = new StagedInterpreterTests
      with PrimitiveOpsExp
      with ArrayOpsExp
      with IfThenElseExp
      with BooleanOpsExp
      with OrderingOpsExp
      with NumericOpsExp
      with WhileExpOptSpeculative
      with VariablesExpOpt
      with EqualExpOpt
      with StringOpsExp
      with ArrayBufferOpsExp
      with FunctionsRecursiveExp
      with MyScalaCompile { self =>
    val codegen = new ScalaGenEffect
        with ScalaGenPrimitiveOps
        with ScalaGenArrayOps
        with ScalaGenIfThenElse
        with ScalaGenBooleanOps
        with ScalaGenOrderingOps
        with ScalaGenWhileOptSpeculative
        with ScalaGenNumericOps
        with ScalaGenEqual
        with ScalaGenFunctions
        with ScalaGenStringOps
        with ScalaGenArrayBufferOps
        with ScalaGenVariables { val IR: self.type = self }

    codegen.emitSource(self.pevalAddTest, "pevalAddTest", new PrintWriter(System.out))
    codegen.reset

    val pevalAddTest : (Int => Int) = compile(self.pevalAddTest)

    codegen.emitSource(self.pevalFactTest, "pevalFactTest", new PrintWriter(System.out))
    codegen.reset

    val pevalFactTest : (Int => Int) = compile(self.pevalFactTest)

  }

  "Addition" should "evaluate dynamically." in {
    peval1(staged.add, env0, fenv0) should be (3)
  }

  "Staged addition" should "evaluate at compile-time to a constant." in {
    staged.pevalAddTest(1) should be (3)
  }

  "Factorial" should "evaluate dynamically." in {
    peval1(staged.fact, env0, fenv0) should be (120)
  }

  "Staged factorial" should "be unwrapped." in {
    staged.pevalFactTest(1) should be (120)
  }
}
