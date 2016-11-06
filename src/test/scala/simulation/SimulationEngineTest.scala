package simulation

import commandResults.IgnoredCommandResult
import commands._
import org.scalatest.{FunSuite, Matchers}
import util.Direction.North

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
      engine.run(MoveCommand).toString should include(" ignored ")
    }
  }

  test("should ignore MOVE before PLACE") {
    new Engine {
      engine.run(MoveCommand).toString should include(" ignored ")
    }
  }

  test("should ignore REPORT before PLACE") {
    new Engine {
      engine.run(ReportCommand).toString should include(" ignored ")
    }
  }

  test("should ignore LEFT and RIGHT before PLACE") {
    new Engine {
      engine.run(LeftCommand).toString should include(" ignored ")
      engine.run(RightCommand).toString should include(" ignored ")
    }
  }

}
