import scala.collection.mutable.ListBuffer
import io.threadcso._

/* Commenting is scarce because the source code itself is self-documenting. */

// this class implements the automaton that emulates conway's game of life 
class Life(sourceUniverse: Life.Universe) extends LifeLikeAutomaton(sourceUniverse) {
    import Life._
    import Life.CellState._

    protected def nextState(pos: Position): CellState = 
    {
        val liveNeighbours = neighboursAlive(pos)

        if((2 > liveNeighbours)  || (3 < liveNeighbours))
        {
            return Dead
        }
        else if(isCellAlive(pos) || 3 == liveNeighbours)
        {
            return Live
        }
        else
        {
            return Dead
        }
    }
}


// this class implements the automaton that emulates the high life varition of conway's game of life 
class HighLife(sourceUniverse: Life.Universe) extends LifeLikeAutomaton(sourceUniverse) {
    import Life._
    import Life.CellState._

    protected def nextState(pos: Position): CellState = 
    {
        val liveNeighbours = neighboursAlive(pos)
        if(isCellAlive(pos))
        {
            if((2 == liveNeighbours) || (3 == liveNeighbours))
            {
                return Live
            }
            else
            {
                return Dead
            }
        }
        else 
        {
            if((3 == liveNeighbours) || (6 == liveNeighbours))
            {
                return Live
            }
            else
            {
                return Dead
            }
        }
    }
}


/* 
this abstract class implements the high level mechanisms of the life-like automatons, except for  
the rules that govern the game. various variations of the game extend this abstract class and implement
these rules. 
*/
abstract class LifeLikeAutomaton(sourceUniverse: Life.Universe) {
    import Life._
    import Life.CellState._

    private var universe: Life.Universe = sourceUniverse
    private val width: Int              = sourceUniverse.length
    private val height: Int             = if(width == 0) -1 else sourceUniverse(0).length

    private var syncBarrier     = new Barrier(10)

    // to be implemented by subclasses
    protected def nextState(pos: Position): CellState


    /*  this method emulates @steps number of steps of the game. 
        it uses four workers which are responsible for emulating the four 
        quarter planes of the universe
    */
    def emulate(steps: Int = -1) = 
    {
        assert(width == height)
        syncBarrier     = new Barrier(4)

        val workers =   quarterPlaneEmulator(Position(0, 0), steps) || quarterPlaneEmulator(Position(0, width / 2), steps) || 
                        quarterPlaneEmulator(Position(width / 2, 0), steps) || quarterPlaneEmulator(Position(width / 2, width / 2), steps)
        workers()
    }


    // this method emulates the game on one quarter (with top left corner at @topLeft) of the universe for @steps number of steps 
    private def quarterPlaneEmulator(topLeft: Position, steps: Int = -1) = proc
    {
        val nextStates: Universe = Array.ofDim[Boolean](width, height)
        val forever = (-1 == steps)
        var stepCounter = steps

        while(forever || stepCounter > 0)
        {
            for(pos <- cellsInQuarterPlane(topLeft))
            {
                nextStates(pos.x)(pos.y) = (Live == nextState(pos))
            }
            
            syncBarrier.sync()

            for(pos <- cellsInQuarterPlane(topLeft))
            {
                universe(pos.x)(pos.y) = nextStates(pos.x)(pos.y)
            }

            stepCounter -= 1
        }
    }



    private def cellsInQuarterPlane(topLeft: Position): List[Position] = 
    {
        val res = new ListBuffer[Position]
        for(x <- 0 until width / 2)
        {
            for(y <- 0 until height / 2)
            {
                res += Position(topLeft.x + x, topLeft.y + y)
            }
        }
        return res.toList
    }



    protected def neighboursAlive(pos: Position): Int = 
    {
        val neighbours =    Position(pos.x-1, pos.y-1)  :: Position(pos.x-1, pos.y) :: Position(pos.x-1, pos.y+1)   :: 
                            Position(pos.x, pos.y-1)    ::                             Position(pos.x, pos.y+1)     :: 
                            Position(pos.x+1, pos.y-1)  :: Position(pos.x+1, pos.y) :: Position(pos.x+1, pos.y+1)   :: Nil
        return neighbours.count(isCellAlive)
    }



    protected def isCellAlive(pos: Position): Boolean = 
    {
        val physicalCoordinates = physicalPosition(pos)
        return universe(physicalCoordinates.x)(physicalCoordinates.y)
    }



    private def physicalPosition(pos: Position): Position = 
    {
        return Position((pos.x + width) % width, (pos.y + height) % height)
    }



    private def kill(pos: Position): Unit = 
    {
        universe(pos.x)(pos.y) = false
    }



    private def start(pos: Position): Unit =
    {
        universe(pos.x)(pos.y) = true
    }



    private def logicalOrOperator(p: Boolean, q: Boolean): Boolean =
    {
        return (p || q)
    }
}



/*  abstract class that implements the tests for life-like automatons -- this is extended by 
    tests for the various variations of the game */
abstract class LifeLikeAutomatonTest
{
    import Life._
    import Life.CellState._

    // to be implemented by subclasses
    protected def generateTestUniverse(): Universe
    protected def instantiateAutomatonForUniverse(universe: Universe): LifeLikeAutomaton

    def main(args: Array[String]) = runTest()

    def runTest(): Unit = 
    {
        val universes   = generateTestUniverses()
        val lives       = universes.map(instantiateAutomatonForUniverse)
        val displays    = universes.map(instantiateDisplayForUniverse)

        while(true)
        {
            universes.map(printUniverse)
            lives.map(_.emulate(1))
            displays.map(_.draw)
            sleep(Sec)
        }
    }



    def generateTestUniverses(): List[Universe] = 
    {
        val universes = ListBuffer[Universe]()
        universes += generateTestUniverse()
        return universes.toList
    }



    protected def generateEmptyUniverseOfDimension(width: Int): Universe = 
    {
        return Array.ofDim[Boolean](width, width)
    }



    private def instantiateDisplayForUniverse(universe: Universe): Display = 
    {
        return new Display(universe.length, universe)
    }



    private def printUniverse(universe: Universe): Unit =
    {
        println()
        for(i <- 0 until universe.length)
        {
            for(j <- 0 until universe.head.length)
            {
                if(universe(i)(j)) print("X") else print("_")
            }
            println()
        }
    }
}



object LifeTest extends LifeLikeAutomatonTest {
    import Life._



    protected def instantiateAutomatonForUniverse(universe: Universe): LifeLikeAutomaton =
    {
        return new Life(universe)
    }



    protected def generateTestUniverse(): Universe = 
    {
        // generates a universe that contains various objects that are described 
        // on the wikipedia page for the game of life
        /* The top few lines are as given below
        ________________________________________
        _XX_____________________________________
        _XX_________XXX_________________________
        ___XX______XXX__________________________
        ___XX___________________________________
        ________________________________________
        ________________________________________
        ________________________________________
        _XXX__XX____X___________________________
        ______XX___X_X__________________________
        ____________X___________________________
        ________________________________________
        ________________________________________
        ________________________________________
        ________________________________________
        __XX____________________________________
        _X__X___________________________________
        __XX____________________________________
        ________________________________________
        ________________________________________
        ________________________________________
        ________________________________________
        __X_____________________________________
        ___X____________________________________
        _XXX____________________________________
        ________________________________________
        */

        val universe = generateEmptyUniverseOfDimension(40)

        // beacon
        universe(1)(1) = true; universe(1)(2) = true; universe(2)(1) = true
        universe(2)(2) = true; universe(3)(3) = true; universe(3)(4) = true
        universe(4)(3) = true; universe(4)(4) = true

        // toad
        universe(2)(12) = true; universe(2)(13) = true; universe(2)(14) = true
        universe(3)(11) = true; universe(3)(12) = true; universe(3)(13) = true

        //blinker 
        universe(3)(18) = true; universe(3)(19) = true; universe(3)(20) = true

        // block
        universe(8)(2) = true; universe(8)(3) = true; universe(9)(2) = true
        universe(9)(3) = true

        // tub
        universe(8)(12) = true; universe(9)(11) = true
        universe(9)(13) = true; universe(10)(12) = true

        // beehive
        universe(8)(20) = true; universe(8)(21) = true; universe(9)(19) = true
        universe(9)(22) = true; universe(10)(20) = true; universe(10)(21) = true

        //glider
        universe(22)(10) = true; universe(23)(11) = true; universe(24)(9) = true
        universe(24)(10) = true; universe(24)(11) = true

        return universe
    }
}


object HighLifeTest extends LifeLikeAutomatonTest {
    import Life._



    protected def instantiateAutomatonForUniverse(universe: Universe): LifeLikeAutomaton =
    {
        return new HighLife(universe)
    }



    protected def generateTestUniverse(): Universe = 
    {
        val universe = generateEmptyUniverseOfDimension(40)

        // the replicator
        universe(20)(22) = true; universe(20)(23) = true; universe(20)(24) = true
        universe(21)(21) = true; universe(21)(24) = true; universe(22)(20) = true
        universe(22)(24) = true; universe(23)(20) = true; universe(23)(23) = true
        universe(24)(20) = true; universe(24)(21) = true; universe(24)(22) = true

        return universe
    }
}



object Life {
    type Universe = Array[Array[Boolean]]
    case class Position(x: Int, y: Int)
    object CellState extends Enumeration {
        type CellState = Value
        val Live, Dead = Value
    }
}



class Display(N: Int, a:Array[Array[Boolean]]) extends java.awt.Frame {
  import java.awt._
  // Define some constants
  private [this] val blockSize = 6
  private [this] val padding   = 1
  private [this] val gridSize  = blockSize+2*padding
  
  // Set up the display
  private [this] val pane = new ScrollPane() 
  pane.setSize(N*gridSize, N*gridSize)
  private [this] val board = new Board()
  pane.add(board)
  this.add(pane, "Center")
  this.pack()
  this.setVisible(true)
  this.setTitle("Life")
  this.setSize(N*gridSize, N*gridSize)
  
  // Fill in all the squares
  def draw = {
    for (i <- 0 until N){
        for (j<-0 until N){
            if (a(i)(j)) board.drawAt(j,i) else board.blankAt(j,i)
        }
    }
  }

  override def paint(g: Graphics) = draw

  class Board extends Component{
    // Define colours
    val backgroundColor = Color.gray.brighter
    val blockColor      = Color.black

    // Paint the square at (x,y) in colour c
    private [this] def paintAt(x: Int, y: Int, c: Color) = {    
      val g = getGraphics()
      g.setColor(c)
      g.fillRect(x*gridSize+padding, y*gridSize+padding, blockSize, blockSize)
    }

    // Draw a piece
    def drawAt(x: Int, y: Int) = paintAt(x, y, blockColor)

    // Draw a blank square
    def blankAt(x: Int, y: Int) = paintAt(x, y, backgroundColor)
  }

}