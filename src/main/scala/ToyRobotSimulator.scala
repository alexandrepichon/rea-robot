import commands._
import simulation.{Report, SimulationEngine}
import lex.LexicalAnalyzer

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object ToyRobotSimulator extends App {
  if (args.length == 1) {
    apply(Source.fromFile(args(0)).getLines().toSeq:_*)
  } else {
    Console.err.println("""
      |Usage: ToyRobotSimulator FILE
      |examples at https://github.com/alexandrepichon/rea-robot/blob/master/PROBLEM.md
    """.stripMargin)
  }

  def apply(commands: String*): String = {
    val lex = new LexicalAnalyzer()

    lex.parse(commands) match {
      case Left(syntaxErrors) =>
        syntaxErrors.map(Console.err.println)
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
