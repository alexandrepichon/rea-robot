package simulation

import util.Direction

case class Report(x: Int, y: Int, direction: Direction){
  override def toString: String = {
    s"$x,$y,$direction"
  }
}
