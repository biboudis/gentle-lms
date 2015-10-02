import org.scalatest._

import scala.lms.common._
import scala.lms.internal._
import scala.language.reflectiveCalls
import java.io.PrintWriter

class PowerSpec extends FlatSpec with Matchers {

  trait PowerSpecTests extends IfThenElse
      with BooleanOps with Variables with OrderingOps
      with PrimitiveOps with LiftVariables with LiftBoolean
      with LiftPrimitives with ArrayOps with ArrayBufferOps
      with While with NumericOps with Equal with LiftString with StringOps {

    // let even n = (n mod 2) = 0;;
    // let square x = x * x;;
    // let rec powerS n x =
    //   if n = 0 then .<1>.
    //   else if even n then .<square .~(powerS (n/2) x)>.
    //   else .<.~x * .~(powerS (n-1) x)>. ;;

    // let power5 = !. .<fun x -> .~(powerS 5 .<x>.)>.;;

    def even (n : Rep[Int]) = n % 2 == 0
    def square (x: Rep[Int]) = x * x
    def powerS (n : Int, x : Rep[Int]) : Rep[Int] = {
      if (n == 0) 1
      else if (even(n)) square(powerS(n/2, x))
      else x * powerS(n-1, x)
    }

    def powerTest(x : Rep[Int]) : Rep[Int] = powerS(5, x)
  }

  val staged = new PowerSpecTests
      with FunctionsRecursiveExp
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
      with FunctionsExp
      with ScalaCompile { self =>
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

    codegen.emitSource(self.powerTest, "powerTest", new PrintWriter(System.out))
    codegen.reset

    val powerTest : (Int => Int) = compile(self.powerTest)

  }

  "Staged power" should "execute the specilized powerS function yielding the correct result" in {
    staged.powerTest(3) should be (243)
  }

}
