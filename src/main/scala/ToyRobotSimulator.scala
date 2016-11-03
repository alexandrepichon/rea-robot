import scala.io.Source

object ToyRobotSimulator extends App {
  apply(Source.fromFile(args(0)).getLines().toSeq:_*)

  def apply(commands: String*): String = {
    ""
  }
}
