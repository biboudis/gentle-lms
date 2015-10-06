import org.scalatest._

import scala.lms.common._
import scala.lms.internal._
import scala.language.reflectiveCalls
import java.io.PrintWriter

class PowerSpec extends FlatSpec with Matchers {

  trait PowerSpecTests extends Variables with PrimitiveOps with LiftPrimitives {
    def even (n: Int) = n % 2 == 0
    def square (x: Rep[Int]) = x * x
    def powerS (n : Int, x : Rep[Int]) : Rep[Int] = {
      if (n == 0) 1
      else if (even(n)) square(powerS(n/2, x))
      else x * powerS(n-1, x)
    }

    def powerTest(x : Rep[Int]) : Rep[Int] = powerS(5, x)
  }

  val staged = new PowerSpecTests with PrimitiveOpsExp with VariablesExpOpt with ScalaCompile { self =>
    val codegen = new ScalaGenPrimitiveOps with ScalaGenVariables { val IR: self.type = self }

    codegen.emitSource(self.powerTest, "powerTest", new PrintWriter(System.out))
    codegen.reset

    val powerTest : (Int => Int) = compile(self.powerTest)
  }

  "Staged power" should "execute the specilized powerS function yielding the correct result" in {
    staged.powerTest(3) should be (243)
  }

}
