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
        if (x < 0 || x > 5) {
          IgnoredCommandResult("PLACE X,Y,DIR: X should be between 0 and 5")
        } else if (y < 0 || y > 5) {
          IgnoredCommandResult("PLACE X,Y,DIR: Y should be between 0 and 5")
        } else {
          xState = Some(x)
          yState = Some(y)
          directionState = Some(direction)
          SilentResult
        }
      case ReportCommand =>
        if (isPlaced()) {
          ReportResult(xState.get, yState.get, directionState.get)
        } else {
          IgnoredCommandResult(s"$ReportCommand ignored: expect $PlaceCommand command before")
        }
      case MoveCommand =>
        runMove()
      case LeftCommand =>
        if (isPlaced()) {
          directionState = directionState.map(_.left)
          SilentResult
        } else {
          IgnoredCommandResult(s"$LeftCommand ignored: expect $PlaceCommand command before")
        }
      case RightCommand =>
        if (isPlaced()) {
          directionState = directionState.map(_.right)
          SilentResult
        } else {
          IgnoredCommandResult(s"$RightCommand ignored: expect $PlaceCommand command before")
        }
    }
  }


  private def isPlaced(): Boolean = {
    xState.isDefined && yState.isDefined && directionState.isDefined
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
