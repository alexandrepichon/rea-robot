package lex

import commands._
import org.scalatest.{FunSuite, Matchers}
import util.Direction._

class LexicalAnalyzerTest extends FunSuite with Matchers {

  val lex = new LexicalAnalyzer

  test("PLACE to nan X position should fail") {
    lex.parse(Seq("PLACE A,2,NORTH")) should be('left)
  }

  test("PLACE to nan Y position should fail") {
    lex.parse(Seq("PLACE 0,Z,NORTH")) should be('left)
  }

  test("PLACE with invalid direction should fail") {
    lex.parse(Seq("PLACE 0,0,LEFT")) should be('left)
  }

  test("PLACE with missing arg should fail") {
    lex.parse(Seq("PLACE 0,0")) should be('left)
  }

  test("PLACE 5,0,EAST should be parsed") {
    lex.parse(Seq("PLACE 5,0,EAST")) should be(Right(Seq(PlaceCommand(5, 0, East))))
  }

  test("MOVE should be parsed") {
    lex.parse(Seq("MOVE")) should be(Right(Seq(MoveCommand)))
  }

  test("LEFT should be parsed"){
    lex.parse(Seq("LEFT")) should be(Right(Seq(LeftCommand)))
  }

  test("RIGHT should be parsed"){
    lex.parse(Seq("RIGHT")) should be(Right(Seq(RightCommand)))
  }

  test("REPORT should be parsed") {
    lex.parse(Seq("REPORT")) should be(Right(Seq(ReportCommand)))
  }

  test("all commands should be parsed") {
    lex.parse(Seq("REPORT","MOVE", "RIGHT", "LEFT", "PLACE 9,9,SOUTH")).right.get should have size 5
  }

  test("all errors should be reported") {
    lex.parse(Seq("REPORT","ERROR", "RIGHT", "LEFT", "TYPO 9,9,SOUTH")).left.get should have size 2
  }

}
