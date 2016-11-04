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
        if (directionState.contains(North)) yState = yState.map(_+1)
        if (directionState.contains(South)) yState = yState.map(_-1)
        if (directionState.contains(East)) xState = xState.map(_+1)
        if (directionState.contains(West)) xState = xState.map(_-1)

        SilentResult
      case LeftCommand =>
        directionState = directionState.map(_.left)
        SilentResult
      case RightCommand =>
        directionState = directionState.map(_.right)
        SilentResult
    }
  }


}
