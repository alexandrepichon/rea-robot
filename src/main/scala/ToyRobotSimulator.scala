import simulation.SimulationEngine
import lex.LexicalAnalyzer

import scala.io.Source

object ToyRobotSimulator extends App {
  apply(Source.fromFile(args(0)).getLines().toSeq:_*)

  def apply(commands: String*): String = {
    val lex = new LexicalAnalyzer()

    lex.parse(commands) match {
      case Left(syntaxErrors) =>
        syntaxErrors.mkString("\n")

      case Right(parsedCommands) =>
        val simulation = new SimulationEngine
        val result = parsedCommands.map { command =>
          simulation.run(command)
        }
        result.mkString("")
    }

  }
}
