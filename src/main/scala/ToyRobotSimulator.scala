import scala.io.Source
import scala.util.Try

object ToyRobotSimulator extends App {
  apply(Source.fromFile(args(0)).getLines().toSeq:_*)

  def apply(commands: String*): String = {
    val PLACE_PREFIX = "PLACE "

    val parsedCommands: Seq[Any] = commands.map { command =>
      if (command.startsWith(PLACE_PREFIX)) {
        val placeArgs:Seq[String] = command.drop(PLACE_PREFIX.length).split(",")
        val tryX = Try { placeArgs(0).toInt }
        val tryY = Try { placeArgs(1).toInt }
        if (tryX.isFailure) {
          IllegalCommand(s"PLACE ?,Y,DIR unexpected X argument in command : $command")
        } else if (tryY.isFailure) {
          IllegalCommand(s"PLACE X,?,DIR unexpected Y argument in command : $command")
        } else {
          PlaceCommand(placeArgs(0).toInt, placeArgs(1).toInt, placeArgs(2))
        }
      } else if (command == "REPORT") {
        ReportCommand
      } else if (command == "MOVE") {
        MoveCommand
      } else if (command == "LEFT") {
        LeftCommand
      } else if (command == "RIGHT") {
        RightCommand
      } else {
        IllegalCommand(command)
      }
    }

    val commandsErrors: Seq[String] = parsedCommands.flatMap { command =>
      command match {
        case IllegalCommand(err) => Seq(err)
        case _ => Seq.empty
      }
    }
    if (commandsErrors.nonEmpty) {
      commandsErrors.mkString("\n")
    } else {
      var xState: Option[Int] = None
      var yState: Option[Int] = None
      var directionState: Option[String] = None
      var result = ""

      parsedCommands.foreach {
        case PlaceCommand(x,y, direction) =>
          xState = Some(x)
          yState = Some(y)
          directionState = Some(direction)
        case ReportCommand =>
          result = s"${xState.get},${yState.get},${directionState.get}"
        case MoveCommand =>
          if (directionState.contains("NORTH")) yState = yState.map(_+1)
          if (directionState.contains("SOUTH")) yState = yState.map(_-1)
          if (directionState.contains("EAST")) xState = xState.map(_+1)
          if (directionState.contains("WEST")) xState = xState.map(_-1)
        case LeftCommand =>
          directionState match {
            case Some("NORTH") => directionState = Some("WEST")
            case Some("SOUTH") => directionState = Some("EAST")
            case Some("EAST") => directionState = Some("NORTH")
            case Some("WEST") => directionState = Some("SOUTH")
          }
        case RightCommand =>
          directionState match {
            case Some("NORTH") => directionState = Some("EAST")
            case Some("SOUTH") => directionState = Some("WEST")
            case Some("EAST") => directionState = Some("SOUTH")
            case Some("WEST") => directionState = Some("NORTH")
          }
        case _ =>
      }
      result
    }


  }
}

case class PlaceCommand(x: Int, y: Int, direction: String)
case object ReportCommand
case object MoveCommand
case object LeftCommand
case object RightCommand
case class IllegalCommand(command: String)
