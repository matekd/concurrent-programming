import io.threadcso._

/* Layout of file
1. The ImageProcessor class that does the actual work
2. Test results
3. The test object that automates testing
*/

class ImageProcessor(val sourceImage: ImageProcessor.Image) {

    case class Position(x: Int, y: Int)

    private var image: ImageProcessor.Image = _

    private val width           = sourceImage.length
    private val height          = if(width == 0) -1 else sourceImage(0).length
    private val pixelCount      = width * height

    private var changeBarrier   = new CombiningBarrier[Boolean](10, false, logicalOrOperator)
    private var syncBarrier     = new Barrier(10)



    def smooth(): ImageProcessor.Image = 
    {
        resetImage()
        val workerCount     = pixelCount
        changeBarrier   = new CombiningBarrier[Boolean](workerCount, false, logicalOrOperator)
        syncBarrier     = new Barrier(workerCount)

        val workers         = || (for (i <- 0 until width; j <- 0 until height) yield pixelSmoother(Position(i, j)))
        workers()
        return image
    }



    private def pixelSmoother(pos: Position) = proc 
    {
        var changeMade: Boolean = true

        while(changeBarrier.sync(changeMade))
        {
            changeMade = (pixelIsSet(pos) != smoothedValue(pos))

            syncBarrier.sync()

            if(changeMade)
            {
                flipPixel(pos)
            }
        }
    }



    private def pixelIsValid(pos: Position): Boolean = 
    {
        return (0 <= pos.x && pos.x < width && 0 <= pos.y && pos.y < height)
    }



    private def pixelIsSet(pos: Position): Boolean = 
    {
        return pixelIsValid(pos) && image(pos.x)(pos.y)
    }



    private def flipPixel(pos: Position): Unit =
    {
        image(pos.x)(pos.y) = !image(pos.x)(pos.y)
    }



    private def populationOfBlock(pos: Position): Int = 
    {
        var population: Int         = 9
        var xOnBorder, yOnBorder    = false

        if(0 == pos.x || width - 1 == pos.x)
        {
            population -= 3
            xOnBorder   = true
        }

        if(0 == pos.y || height - 1 == pos.y)
        {
            population -= 3
            yOnBorder   = true
        }

        if(xOnBorder && yOnBorder)
        {
            population += 1
        }

        return population
    }



    private def pixelsSetInBlock(pos: Position): Int = 
    {
        val neighbours =    Position(pos.x-1, pos.y-1)  :: Position(pos.x-1, pos.y) :: Position(pos.x-1, pos.y+1)   :: 
                            Position(pos.x, pos.y-1)    :: Position(pos.x, pos.y)   :: Position(pos.x, pos.y+1)     :: 
                            Position(pos.x+1, pos.y-1)  :: Position(pos.x+1, pos.y) :: Position(pos.x+1, pos.y+1)   :: Nil
        return neighbours.count(pixelIsSet)
    }



    private def smoothedValue(pos: Position): Boolean =
    {
        return (populationOfBlock(pos) * 0.5 < pixelsSetInBlock(pos))
    }



    private def logicalOrOperator(p: Boolean, q: Boolean): Boolean =
    {
        return (p || q)
    }



    private def resetImage(): Unit =
    {
        image = sourceImage.map(_.clone)
    }
}




object ImageProcessor 
{
    type Row    = Array[Boolean]
    type Image  = Array[Row]
}


/* Test output

Running test on image of dimensions: 17 x 7...
░█░█░██      ░░░░░░░
█░░░█░█      ░░░░░░░
███░░█░      ░░░░░░░
░░░█░█░      ░░░░░░░
░░██░░░      ░███░░░
██░███░      █████░░
██░█░█░      ███████
░█░█░██      ░░█████
░░░█░░█      ░░░░███
██░█░██      ░░░░░░░
░██░░██      ░░░░░░░
░░█░░░░      ░░░░░░░
██░█░█░      ░░░░░░░
█░░░██░      ██░░░░░
█░██░░░      █████░░
████░█░      ██████░
█░░████      ██████░
Running test on image of dimensions: 9 x 8...
░██░░░██      ░░░░░░░░
░░░░█░░█      ░░░░░░░░
░░██░░██      ░░░░░░░░
█░░░█░░█      ░░░░░░░░
█░█████░      ░░░░░░░░
███░█░░█      ░░░░░░░░
█░░░█░░█      ░░░░░░░░
░░█░░░█░      ░░░░░░░░
██░░░░██      ░░░░░░░░
Running test on image of dimensions: 12 x 15...
█░███░░█░██░░░█      ░░░░░░░░████░░░
█░░░░█░░██████░      ░░░░░░░░█████░░
░█░██░░░█████░░      █████░░░█████░░
████░███░█░░░░░      ██████░░█████░░
█░████░░██░███░      ██████░░░███░░░
░█░░█░░░█░█░██░      ░█████░░░░░░░░░
░█░░███░██░░░█░      ░░████░░░░░░░░░
█░██░█░░░█░██░█      ░░█████░░░░░░░░
░░██░██░█░░░░█░      ░██████░░░░░░░░
░██████░░░█░███      ███████░░░░░░░░
░██░░███░░░█░░█      ███████░░░░░░░░
████░█░░█░░░███      ██████░░░░░░░░░
*/



object ImageProcessorTest
{
    private val minWidth    = 5
    private val maxWidth    = 20
    private val minHeight   = 5
    private val maxHeight   = 20
    private val testCount   = 3

    private val whitePixel: Char  = '░'
    private val blackPixel: Char  = '█'




    def main(args: Array[String]) = runTests()



    def runTests(): Unit = 
    {
        println("Running " + testCount + " random tests...")
        val images = generateRandomImages(testCount)
        images.foreach(testImage)
    }



    def testImage(image: ImageProcessor.Image): Unit = 
    {
        println("Running test on image of dimensions: " + image.length + " x " + image.head.length + "...")
        
        val processor = new ImageProcessor(image)
        if(image == processor.smooth())
        {
            println("lofasz")
        }
        printImagesSideBySide(image, processor.smooth())
    }



    def printImagesSideBySide(l: ImageProcessor.Image, r: ImageProcessor.Image): Unit = 
    {
        for(i <- 0 until l.length)
        {
            for(j <- 0 until l.head.length)
            {
                if(l(i)(j))
                {
                    print(blackPixel)
                }
                else
                {
                    print(whitePixel)
                }
            }

            print("      ")

            for(j <- 0 until r.head.length)
            {
                if(r(i)(j))
                {
                    print(blackPixel)
                }
                else
                {
                    print(whitePixel)
                }
            }
        println()
        }
    }



    private def getRandomIntegerInRange(l: Int, h: Int): Int = 
    {
        val rnd = new scala.util.Random
        return l + rnd.nextInt(h - l + 1)
    }



    private def getRandomBoolean(): Boolean =
    {
        return (0 == getRandomIntegerInRange(0, 1))
    }



    private def getRandomWidth(): Int = 
    {
        return getRandomIntegerInRange(minWidth, maxWidth)
    }



    private def getRandomHeight(): Int =
    {
        return getRandomIntegerInRange(minHeight, maxHeight)
    }



    private def generateRandomImage(): ImageProcessor.Image = 
    {
        val width   = getRandomWidth()
        val height  = getRandomHeight()
        val image   = new Array[ImageProcessor.Row](width)

        for(x <- 0 until width)
        {
            image(x) = new Array[Boolean](height)
            for(y <- 0 until height)
            {
                image(x)(y) = getRandomBoolean()
            }
        }

        return image
    }



    private def generateRandomImages(imageCount: Int): Array[ImageProcessor.Image] =
    {
        val images = new Array[Int](imageCount)
        return images.map(_ => generateRandomImage)
    }
}