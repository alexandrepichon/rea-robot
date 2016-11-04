package commands

import util.Direction

sealed trait Command
case class PlaceCommand(x: Int, y: Int, direction: Direction) extends Command
case object ReportCommand extends Command
case object MoveCommand extends Command
case object LeftCommand extends Command
case object RightCommand extends Command