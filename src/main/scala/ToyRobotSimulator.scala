import commands._
import simulation.{Report, SimulationEngine}
import lex.LexicalAnalyzer

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object ToyRobotSimulator extends App {
  apply(Source.fromFile(args(0)).getLines().toSeq:_*)

  def apply(commands: String*): String = {
    val lex = new LexicalAnalyzer()
    
    lex.parse(commands) match {
      case Left(syntaxErrors) =>
        syntaxErrors.map(println)
        syntaxErrors.mkString("\n")

      case Right(parsedCommands) =>
        runCommands(parsedCommands)
    }
  }

  private def runCommands(commands: Seq[Command]): String = {
    val simulation = new SimulationEngine
    val reports = new ArrayBuffer[Report]()
    val printError : PartialFunction[Throwable, Unit] = {case e => println(e.getMessage)}

    commands.foreach {
      case PlaceCommand(x, y, direction) => simulation.place(x, y, direction).recover(printError)
      case MoveCommand => simulation.move().recover(printError)
      case LeftCommand => simulation.left().recover(printError)
      case RightCommand => simulation.right().recover(printError)
      case ReportCommand => simulation.report().map { report =>
          println(report)
          reports.append(report)
        }.recover(printError)
    }
    reports.mkString("\n")
  }
}
