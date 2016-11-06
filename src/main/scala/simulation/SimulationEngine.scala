package simulation

import commandResults._
import commands._
import util.Direction
import util.Direction._

class SimulationEngine {
  private val TABLE_MIN = 0
  private val TABLE_MAX = 5


  private var xState: Option[Int] = None
  private var yState: Option[Int] = None
  private var directionState: Option[Direction] = None

  def run(command: Command): CommandResult = {
    command match {
      case PlaceCommand(x,y, direction) =>
        if (x < TABLE_MIN || x > TABLE_MAX) {
          IgnoredCommandResult(s"PLACE X,Y,DIR: X should be between $TABLE_MIN and $TABLE_MAX")
        } else if (y < TABLE_MIN || y > TABLE_MAX) {
          IgnoredCommandResult(s"PLACE X,Y,DIR: Y should be between $TABLE_MIN and $TABLE_MAX")
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
      case (Some(North), _, Some(TABLE_MAX)) =>
        IgnoredCommandResult(s"$MoveCommand to $North ignored: robot already at the top")
      case (Some(South), _, Some(TABLE_MIN)) =>
        IgnoredCommandResult(s"$MoveCommand to $South ignored: robot already at the bottom")
      case (Some(East), Some(TABLE_MAX), _) =>
        IgnoredCommandResult(s"$MoveCommand to $East ignored: robot already at the right")
      case (Some(West), Some(TABLE_MIN), _) =>
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
