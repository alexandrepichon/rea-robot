package simulation

import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.{FunSuite, Matchers}
import util.Direction.{North, South, West}
import CommandResultMatchers._

import scala.util.Try

class SimulationEngineTest extends FunSuite with Matchers {

  // Fixtures
  trait Engine {
    val engine = new SimulationEngine()
  }

  trait EngineAt00N extends Engine {
    engine.place(0, 0, North)
  }


  // Tests
  test("should ignore the sixth move") {
    new EngineAt00N {
      (1 to 5).foreach(_ => engine.move())
      engine.move() should be(ignored)
    }
  }

  test("should ignore MOVE before PLACE") {
    new Engine {
      engine.move() should be(ignored)
    }
  }

  test("should ignore REPORT before PLACE") {
    new Engine {
      an [CommandIgnoredException] should be thrownBy {
        engine.report().get
      }
    }
  }

  test("should ignore LEFT and RIGHT before PLACE") {
    new Engine {
      engine.left() should be(ignored)
      engine.left() should be(ignored)
    }
  }

  test("REPORT should give robot position") {
    new EngineAt00N {
      engine.report().get should be(Report(0, 0, North))
    }
  }

  test("can PLACE twice") {
    new EngineAt00N {
      engine.place(5,5,West)
      engine.report().get should be(Report(5, 5, West))
    }
  }

  test("PLACE outside the table should be ignored") {
    new Engine {
      engine.place(6,0,South) should be(ignored)
      engine.place(3,100,South) should be(ignored)
      engine.place(-1,4,South) should be(ignored)
      engine.place(5,-2,South) should be(ignored)
    }
  }

}

trait CommandResultMatchers {

  class IgnoredMatcher extends BeMatcher[Try[Unit]] {
    def apply(left: Try[Unit]) =
      MatchResult(
        left.isFailure,
        left + " was not ignored",
        left.toString + " was ignored"
      )
  }
  def ignored = new IgnoredMatcher
}
object CommandResultMatchers extends CommandResultMatchers

