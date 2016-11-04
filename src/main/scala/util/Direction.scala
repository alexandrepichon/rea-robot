package util

sealed trait Direction {
  def label: String
  def left: Direction
  def right: Direction
  override def toString: String = label
}

object Direction {
  case object North extends Direction {
    val label = "NORTH"
    val left = West
    val right = East
  }
  case object South extends Direction {
    val label = "SOUTH"
    val left = East
    val right = West
  }
  case object East extends Direction {
    val label = "EAST"
    val left = North
    val right = South
  }
  case object West extends Direction {
    val label = "WEST"
    val left = South
    val right = North
  }

  val directions = Set(North, South, East, West)

  def apply(direction: String): Option[Direction] = {
    directions.find(_.label == direction)
  }
}
