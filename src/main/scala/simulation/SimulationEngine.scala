package simulation

import commandResults._
import commands._
import util.Direction
import util.Direction._

class SimulationEngine {

  var xState: Option[Int] = None
  var yState: Option[Int] = None
  var directionState: Option[Direction] = None

  def run(command: Command): CommandResult = {
    command match {
      case PlaceCommand(x,y, direction) =>
        xState = Some(x)
        yState = Some(y)
        directionState = Some(direction)
        SilentResult
      case ReportCommand =>
        ReportResult(xState.get, yState.get, directionState.get)
      case MoveCommand =>
        runMove()
      case LeftCommand =>
        directionState = directionState.map(_.left)
        SilentResult
      case RightCommand =>
        directionState = directionState.map(_.right)
        SilentResult
    }
  }


  private def runMove(): CommandResult = {
    (directionState, xState, yState) match {
      case (Some(North), _, Some(5)) =>
        IgnoredCommandResult(s"$MoveCommand to $North ignored: robot already at the top")
      case (Some(South), _, Some(0)) =>
        IgnoredCommandResult(s"$MoveCommand to $South ignored: robot already at the bottom")
      case (Some(East), Some(5), _) =>
        IgnoredCommandResult(s"$MoveCommand to $East ignored: robot already at the right")
      case (Some(West), Some(0), _) =>
        IgnoredCommandResult(s"$MoveCommand to $West ignored: robot already at the left")

      case (None, None, None) =>
        IgnoredCommandResult(s"$MoveCommand ignored: expect $PlaceCommand command before")

      case (Some(North), _, Some(y)) =>
        yState = Some(y + 1)
        SilentResult
      case (Some(South), _, Some(y)) =>
        yState = Some(y - 1)
        SilentResult
      case (Some(East), Some(x), _) =>
        xState = Some(x + 1)
        SilentResult
      case (Some(West), Some(x), _) =>
        xState = Some(x - 1)
        SilentResult

      case _ => ???
    }
  }
}
