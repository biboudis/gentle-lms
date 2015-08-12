import lms._
import interpreters._
import org.scalatest._

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.language.reflectiveCalls
import java.io.PrintWriter

class StagedInterpreterSpec extends FlatSpec with Matchers {

  import interpreters.BasicInterpreter._

  "Simple addition" should "evaluate properly." in {
    val prog = Program(Nil, Add(IntVal(1), IntVal(2)))
    peval1(prog, env0, fenv0) should be (3)
  }
}
