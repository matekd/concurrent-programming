import math._
import io.threadcso._


/* Test output snippet:
Multiplying matrices...
Left: 
Array(1, 2)
Array(3, 4)
Right: 
Array(5, 6)
Array(7, 8)
Result: 
Array(19, 22)
Array(43, 50)
Multiplying matrices...
Left: 
Array(1, 2)
Array(3, 4)
Right: 
Array(1, 2)
Array(3, 4)
Result: 
Array(7, 10)
Array(15, 22)
*/


object MatrixMultiplier {

    type Matrix     = Array[Array[Int]]
    type Vector     = Array[Int]
    case class Position(i: Int, j: Int)
    case class VectorPair(left: Vector, right: Vector)
    case class CellComputationJob(pos: Position, vectors: VectorPair)
    case class CellValue(pos: Position, value: Int)

    private val defaultWorkerCount = 10 



    def multiply(left: Matrix, right: Matrix, res: Matrix)
    {
        assertMatrixDimensionsMatch(left, right)
        multiplicationCoordinator(left, right, res)
    }



    private def multiplicationCoordinator(left: Matrix, right: Matrix, res: Matrix): Unit =  
    {
        val workerCount     = min(left.length * left.length, defaultWorkerCount)
        val jobChannel      = N2N[CellComputationJob](1, workerCount)
        val resChannel      = N2N[CellValue](workerCount, 1)

        val workers = || (for (i <- 0 until workerCount) yield cellCalculator(jobChannel, resChannel))
        val coordinatorComposition = (workers || jobAllocator(left, right, jobChannel) || resultCollector(res, resChannel))
        coordinatorComposition()
    }



    private def jobAllocator(left: Matrix, right: Matrix, jobChannel: ![CellComputationJob]) = proc
    {
        val transposedRight = right.transpose

        for(i <- 0 until left.length)
        {
            for(j <- 0 until left.length)
            {
                val pos     = Position(i, j)
                val vectors = VectorPair(left(i), transposedRight(j))
                jobChannel!(CellComputationJob(pos, vectors)) 
            }
        }

        jobChannel.closeOut
    }



    private def resultCollector(res: Matrix, resChannel: ?[CellValue]) = proc
    {
        repeat
        {
            val cellResult: CellValue = resChannel?()
            res(cellResult.pos.i)(cellResult.pos.j) = cellResult.value
        }

        resChannel.closeIn
    }



    private def cellCalculator(jobChannel: ?[CellComputationJob], resChannel: ![CellValue]) = proc 
    {
        repeat
        {
            val job = jobChannel?()
            val result = scalarProduct(job.vectors.left, job.vectors.right)
            resChannel!(CellValue(job.pos, result))
        }

        jobChannel.closeIn
        resChannel.closeOut
    }



    private def scalarProduct(left: Vector, right: Vector): Int = 
    {
        assertVectorDimensionsMatch(left, right)
        return (left zip right).foldLeft(0) {(total, pair) => total + pair._1 * pair._2}
    }



    private def assertVectorDimensionsMatch(left: Vector, right: Vector): Unit =
    {
        assert(left.length == right.length)
    }



    private def assertMatrixDimensionsMatch(left: Matrix, right: Matrix): Unit =
    {
        assert(0 != left.length)
        assert(left.length == left(0).length)
        assert(left.length == right.length)
        assert(left(0).length == right(0).length)
    }
}



object MatrixMultiplierTest {
    val testMatrices: Array[MatrixMultiplier.Matrix] = Array(
                        Array(Array(1, 2), Array(3, 4)), 
                        Array(Array(5, 6), Array(7, 8)),
                        Array(Array(1,2,3,4), Array(5,6,7,8), Array(9, 10, 11, 12), Array(13, 14, 15, 16)),
                        Array(Array(1,0,0,0), Array(0,1,0,0), Array(0,0,1,0), Array(0,0,0,1))
                    )

    def main(args: Array[String]) = {runTests()}


    def runTests(): Unit = 
    {
        runTestOnMatrices(0, 1)
        runTestOnMatrices(0, 0)
        runTestOnMatrices(2, 2)
        runTestOnMatrices(2, 3)
        runTestOnMatrices(3, 3)
    }



    private def runTestOnMatrices(leftMatrixId: Int, rightMatrixId: Int): Unit = 
    {
        val left    = testMatrices(leftMatrixId)
        val right   = testMatrices(rightMatrixId)
        val res     = Array.ofDim[Int](left.length, left.length)

        println("Multiplying matrices...")
        println("Left: "); printMatrix(left)
        println("Right: "); printMatrix(right)
        
        MatrixMultiplier.multiply(left, right, res)
        println("Result: "); printMatrix(res)
    }



    private def printMatrix(matrix: MatrixMultiplier.Matrix): Unit = 
    {
        println(matrix.deep.mkString("\n"))
    }
}