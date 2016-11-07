package simulation

import commands._
import util.Direction
import util.Direction._

import scala.util.{Failure, Success, Try}

class SimulationEngine {
  private val TABLE_MIN = 0
  private val TABLE_MAX = 5

  private var xState: Option[Int] = None
  private var yState: Option[Int] = None
  private var directionState: Option[Direction] = None

  def place(x: Int, y: Int, direction: Direction): Try[Unit] = {
    if (x < TABLE_MIN || x > TABLE_MAX) {
      ignore(s"PLACE X,Y,DIR: X should be between $TABLE_MIN and $TABLE_MAX")
    } else if (y < TABLE_MIN || y > TABLE_MAX) {
      ignore(s"PLACE X,Y,DIR: Y should be between $TABLE_MIN and $TABLE_MAX")
    } else {
      xState = Some(x)
      yState = Some(y)
      directionState = Some(direction)
      Success(())
    }
  }

  def move(): Try[Unit] = {
    (directionState, xState, yState) match {
      case (Some(North), _, Some(TABLE_MAX)) =>
        ignore(s"$MoveCommand to $North ignored: robot already at the top")
      case (Some(South), _, Some(TABLE_MIN)) =>
        ignore(s"$MoveCommand to $South ignored: robot already at the bottom")
      case (Some(East), Some(TABLE_MAX), _) =>
        ignore(s"$MoveCommand to $East ignored: robot already at the right")
      case (Some(West), Some(TABLE_MIN), _) =>
        ignore(s"$MoveCommand to $West ignored: robot already at the left")

      case (None, None, None) =>
        ignore(s"$MoveCommand ignored: expect $PlaceCommand command before")

      case (Some(North), _, Some(y)) =>
        yState = Some(y + 1)
        Success()
      case (Some(South), _, Some(y)) =>
        yState = Some(y - 1)
        Success()
      case (Some(East), Some(x), _) =>
        xState = Some(x + 1)
        Success()
      case (Some(West), Some(x), _) =>
        xState = Some(x - 1)
        Success()

      case _ => ???
    }
  }

  def left(): Try[Unit] = {
    if (isPlaced()) {
      directionState = directionState.map(_.left)
      Success()
    } else {
      ignore(s"$LeftCommand ignored: expect $PlaceCommand command before")
    }
  }

  def right(): Try[Unit] = {
    if (isPlaced()) {
      directionState = directionState.map(_.right)
      Success()
    } else {
      ignore(s"$RightCommand ignored: expect $PlaceCommand command before")
    }
  }

  def report(): Try[Report] = {
    if (isPlaced()) {
      Success(Report(xState.get, yState.get, directionState.get))
    } else {
      ignore(s"$ReportCommand ignored: expect $PlaceCommand command before")
    }
  }


  private def isPlaced(): Boolean = {
    xState.isDefined && yState.isDefined && directionState.isDefined
  }

  private def ignore[T](message: String): Failure[T] = {
    Failure(new CommandIgnoredException(message))
  }

}
