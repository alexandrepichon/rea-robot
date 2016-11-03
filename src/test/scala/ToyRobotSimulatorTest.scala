import org.scalatest._

class ToyRobotSimulatorTest extends FlatSpec with Matchers {

  /**
    The application is a simulation of a toy robot moving on a square tabletop, of dimensions 5 units x 5 units.
    There are no other obstructions on the table surface.
    The robot is free to roam around the surface of the table, but must be prevented from falling to destruction. Any movement that would result in the robot falling from the table must be prevented, however further valid movement commands must still be allowed.
    Create an application that can read in commands of the following form:
    PLACE X,Y,F
    MOVE
    LEFT
    RIGHT
    REPORT
    */
	"The application" should "read in commands of the following form PLACE X,Y,F\n MOVE\n LEFT\n RIGHT\n REPORT" in {
    ToyRobotSimulator("PLACE X,Y,F", "MOVE", "LEFT", "RIGHT", "REPORT") should not be 'empty
	}

  "Example a" should "report 0,1,NORTH" in {
    ToyRobotSimulator("PLACE 0,0,NORTH", "MOVE", "REPORT") should be("0,1,NORTH")
  }

  "Example b" should "report 0,0,WEST" in {
    ToyRobotSimulator("PLACE 0,0,NORTH", "LEF", "REPORT") should be("0,0,WEST")
  }

  "Example c" should "report 3,3,NORTH" in {
    ToyRobotSimulator("PLACE 1,2,EAST", "MOVE", "MOVE", "LEFT", "MOVE", "REPORT") should be("3,3,NORTH")
  }

  "ToyRobotSimulator" should "read a file input" in {
    ToyRobotSimulator.main(Array("./commands.txt")) should be(())
  }

}