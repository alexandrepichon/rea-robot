package simulation

import commandResults.{CommandResult, IgnoredCommandResult, ReportResult}
import commands._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.{FunSuite, Matchers}
import util.Direction.{North, South, West}
import CommandResultMatchers._

class SimulationEngineTest extends FunSuite with Matchers {

  // Fixtures
  trait Engine {
    val engine = new SimulationEngine()
  }

  trait EngineAt00N extends Engine {
    engine.run(PlaceCommand(0, 0, North))
  }



  // Tests
  test("should ignore the sixth move") {
    new EngineAt00N {
      (1 to 5).foreach(_ => engine.run(MoveCommand))
      engine.run(MoveCommand) should be(ignored)
    }
  }

  test("should ignore MOVE before PLACE") {
    new Engine {
      engine.run(MoveCommand) should be(ignored)
    }
  }

  test("should ignore REPORT before PLACE") {
    new Engine {
      engine.run(ReportCommand) should be(ignored)
    }
  }

  test("should ignore LEFT and RIGHT before PLACE") {
    new Engine {
      engine.run(LeftCommand) should be(ignored)
      engine.run(RightCommand) should be(ignored)
    }
  }

  test("REPORT should give robot position") {
    new EngineAt00N {
      engine.run(ReportCommand) should be(ReportResult(0, 0, North))
    }
  }

  test("can PLACE twice") {
    new EngineAt00N {
      engine.run(PlaceCommand(5,5,West))
      engine.run(ReportCommand) should be(ReportResult(5, 5, West))
    }
  }

  test("PLACE outside the table should be ignored") {
    new Engine {
      engine.run(PlaceCommand(6,0,South)) should be(ignored)
      engine.run(PlaceCommand(3,100,South)) should be(ignored)
      engine.run(PlaceCommand(-1,4,South)) should be(ignored)
      engine.run(PlaceCommand(5,-2,South)) should be(ignored)
    }
  }

}

trait CommandResultMatchers {

  class IgnoredMatcher extends BeMatcher[CommandResult] {
    def apply(left: CommandResult) =
      MatchResult(
        left.toString.contains(" ignored "),
        left.getClass.getSimpleName + " was not ignored",
        left.toString + " was ignored"
      )
  }
  val ignored = new IgnoredMatcher
}
object CommandResultMatchers extends CommandResultMatchers

