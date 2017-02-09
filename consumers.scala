import io.threadcso._
import io.threadcso.semaphore._

/* 

This implementation is somewhat more efficient than the previous implemenation since the critical section of the 
put / get methods are reduced to the parts manipulating the buffers only.

Output from test run:

Producer 3 inserts 0
Producer 1 inserts 9
Producer 2 inserts 0
Producer 4 inserts 8
Producer 2 inserts 8
Producer 5 inserts 2
Producer 2 inserts 6
Producer 3 inserts 3
Producer 1 inserts 1
Producer 1 inserts 5
Producer 2 inserts 1
Producer 4 inserts 3
Consumer gets ListBuffer(3000, 1009, 2000, 4008, 2008, 5002, 2006, 3003, 1001, 1005) at 161
Producer 3 inserts 4
Producer 1 inserts 1
Producer 5 inserts 5
Producer 2 inserts 9
Producer 2 inserts 4
Producer 3 inserts 9
Producer 1 inserts 2
Producer 5 inserts 3
Consumer gets ListBuffer(2001, 4003, 5005, 3004, 1001, 2009, 2004, 3009, 1002, 5003) at 312

*/

object ConsumerM {
    object Buffer {
        
        object WritingPhase extends Enumeration {
            type WritingPhase = Value 
            val Alpha, Beta = Value 
        }
        
        import WritingPhase._

        private val fakeMutex               = new BooleanSemaphore(available = true)
        private val activeBufferIsFull      = new BooleanSemaphore(available = false)
        private val activeBufferIsNotFull   = new BooleanSemaphore(available = true)

        private val BufferMaxSize       = 10
        private val bufferAlpha         = new Array[Int](BufferMaxSize)
        private val bufferBeta          = new Array[Int](BufferMaxSize)

        private var bufferSize          = 0
        private var phase: WritingPhase = Alpha

        private def lockMutex(): Unit   = fakeMutex.acquire()
        private def unlockMutex(): Unit = fakeMutex.release()


        private def swapPhase(): Unit = 
        {
            if(Alpha == phase)
            {
                phase = Beta
            }
            else
            {
                phase = Alpha
            }
            bufferSize = 0
        }



        private def isActiveBufferFull(): Boolean =
        {
            return (bufferSize >= BufferMaxSize)
        }



        private def getActiveBuffer(): Array[Int] = 
        {
            if(Alpha == phase)
            {
                return bufferAlpha
            }
            else
            {
                return bufferBeta
            }
        }



        def put(x: Int): Unit =  
        {
            waitUntilBufferIsNotFull()

            lockMutex()
            val buffer = getActiveBuffer()
            buffer(bufferSize) = x
            bufferSize        += 1
            unlockMutex()

            signalBufferStatus()
        }



        def get(): Array[Int] =  
        {
            waitUntilBufferIsFull()

            lockMutex()
            val res = getActiveBuffer()
            swapPhase()
            unlockMutex()

            signalBufferStatus()
            return res
        }


        private def waitUntilBufferIsFull(): Unit = 
        {
            while(!isActiveBufferFull())
            {
                activeBufferIsFull.acquire()
            }
        }



        private def waitUntilBufferIsNotFull(): Unit = 
        {
            while(isActiveBufferFull())
            {
                activeBufferIsNotFull.acquire()
            }
        }



        private def signalBufferStatus(): Unit =
        {
            if(isActiveBufferFull)
            {
                activeBufferIsFull.release()
            }
            else
            {
                activeBufferIsNotFull.release()
            }
        }
    }

    val random = new scala.util.Random;

    def Producer(me: Int) = proc("Producer"+me) {
        while(true)
        {
            sleep(random.nextInt(100)*milliSec)
            val item = random.nextInt(10)
            Buffer.put(me*1000+item)
            println(s"Producer ${me} inserts ${item}")
        }
    }



    val Consumer = proc("Consumer") {
        val start = milliTime
        while(true)
        {
            val result = Buffer.get()
            val res    = new scala.collection.mutable.ListBuffer[Int]
            for(r <- result) 
            {
                res.append(r)
            }
            println(s"Consumer gets ${res} at ${(milliTime - start)/10}")
            sleep(milliSec * 1500)
        }
    }



    val prods = 5
    val System = Consumer || (|| (for (i <- 0 until prods) yield Producer(i+1)))
    def main(args: Array[String]) = System()
}