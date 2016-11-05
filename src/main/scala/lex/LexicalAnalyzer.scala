package lex

import commands._
import util.Direction

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

class LexicalAnalyzer {

  private val PLACE_PREFIX = "PLACE "

  def parse(commands: Seq[String]): Either[Seq[SyntaxError], Seq[Command]] = {
    val parsedCommands = new ArrayBuffer[Command]()
    val errors = new ArrayBuffer[SyntaxError]()

    commands.foreach { command =>
      parseCommand(command) match {
        case Left(syntaxError) => errors.append(syntaxError)
        case Right(parsedCommand) => parsedCommands.append(parsedCommand)
      }
    }

    if(errors.isEmpty) {
      Right(parsedCommands.toList)
    } else {
      Left(errors.toList)
    }
  }

  def parseCommand(command: String): Either[SyntaxError, Command] = {
    if (command.startsWith(PLACE_PREFIX)) {
      parsePlaceCommand(command)
    } else if (command == "REPORT") {
      Right(ReportCommand)
    } else if (command == "MOVE") {
      Right(MoveCommand)
    } else if (command == "LEFT") {
      Right(LeftCommand)
    } else if (command == "RIGHT") {
      Right(RightCommand)
    } else {
      Left(SyntaxError(s"Unknown command : $command"))
    }
  }

  def parsePlaceCommand(command: String): Either[SyntaxError, PlaceCommand] with Product with Serializable = {
    val placeArgs: Seq[String] = command.drop(PLACE_PREFIX.length).split(",")
    val tryX = Try(placeArgs(0).toInt)
    val tryY = Try(placeArgs(1).toInt)
    val opDirection = Direction(placeArgs(2))
    if (tryX.isFailure) {
      Left(SyntaxError(s"PLACE X,Y,DIR unexpected X argument in command : $command"))
    } else if (tryY.isFailure) {
      Left(SyntaxError(s"PLACE X,Y,DIR unexpected Y argument in command : $command"))
    } else if (opDirection.isEmpty) {
      Left(SyntaxError(s"PLACE X,Y,DIR unexpected DIR argument in command : $command"))
    } else {
      Right(PlaceCommand(tryX.get, tryY.get, opDirection.get))
    }
  }
}
