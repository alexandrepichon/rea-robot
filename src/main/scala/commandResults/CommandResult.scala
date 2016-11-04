package commandResults

import util.Direction

sealed trait CommandResult
case object SilentResult extends CommandResult {
  override def toString: String = ""
}
case class ReportResult(x: Int, y: Int, direction: Direction) extends CommandResult {
  override def toString: String = s"$x,$y,$direction"
}