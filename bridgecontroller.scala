import io.threadcso._



trait BridgeControllerSkeleton {
    def arriveNorth(): String 
    def arriveSouth(): String
    def leaveNorth():  String
    def leaveSouth():  String
}



object BridgeController {
    object Direction extends Enumeration {
        type Direction = Value
        val NorthBound, SouthBound = Value
    }
}



class MonitorBridgeController extends BridgeControllerSkeleton {
    import BridgeController.Direction._

    private [this] var numberOfCarsTravellingNortBound: Int     = 0 
    private [this] val monitor                                  = new Monitor
    private [this] val bridgeIsClearMonitor                     = monitor.newCondition


    private val start           = nanoTime
    private def now             = (nanoTime-start)/1E9
    private def elapsedTime()   = (f"$now%6.5f") 



    def arriveNorth(): String = monitor withLock
    {
        getPermissionToCross(SouthBound)
        return elapsedTime()
    }



    def arriveSouth(): String = monitor withLock
    {
        getPermissionToCross(NorthBound)
        return elapsedTime()
    }



    def leaveSouth(): String = monitor withLock
    {
        leaveBridge(SouthBound)
        val t = elapsedTime()
        bridgeIsClearMonitor.signal()
        return t
    }



    def leaveNorth(): String = monitor withLock
    {
        leaveBridge(NorthBound)
        val t = elapsedTime()
        bridgeIsClearMonitor.signal()
        return t
    }



    private def getPermissionToCross(direction: Direction): Unit = 
    {
        if(NorthBound == direction)
        {
            while(0 > numberOfCarsTravellingNortBound)
            {
                bridgeIsClearMonitor.await()
            }
        }
        else
        {
            while(0 < numberOfCarsTravellingNortBound)
            {
                bridgeIsClearMonitor.await()
            }
        }

        registerBridgeUser(direction)
    }



    private def signalIfBridgeIsClear(): Unit =
    {
        if(0 == numberOfCarsTravellingNortBound)
        {
            bridgeIsClearMonitor.signal()
        }
    }



    private def registerBridgeUser(direction: Direction)
    {
        if(NorthBound == direction)
        {
            numberOfCarsTravellingNortBound += 1
        }
        else
        {
            numberOfCarsTravellingNortBound -= 1
        }
    }



    private def deregisterBridgeUser(direction: Direction)
    {
        if(NorthBound == direction)
        {
            numberOfCarsTravellingNortBound -= 1
        }
        else
        {
            numberOfCarsTravellingNortBound += 1
        }
    }



    private def leaveBridge(direction: Direction)
    {
        deregisterBridgeUser(direction)
    }
}



/* Test output for the monitor based implementation:

Starting traffic...
Cars travelling NorthBound
                              Cars travelling SouthBound
Car #0 arrived
                              Car #0 arrived
Car #0 entered the bridge
Car #0 left the bridge
                              Car #0 entered the bridge
                              Car #0 left the bridge
Car #1 arrived
Car #1 entered the bridge
                              Car #1 arrived
Car #1 left the bridge
                              Car #1 entered the bridge
                              Car #1 left the bridge
Car #2 arrived
Car #2 entered the bridge
                              Car #2 arrived
Car #2 left the bridge
                              Car #2 entered the bridge
Car #3 arrived
Car #3 entered the bridge
                              Car #2 left the bridge
                              Car #3 arrived
Car #3 left the bridge
                              Car #3 entered the bridge
                              Car #3 left the bridge
Car #4 arrived
Car #4 entered the bridge
                              Car #4 arrived
Car #4 left the bridge
                              Car #4 entered the bridge
                              Car #4 left the bridge
                              Car #5 arrived
                              Car #5 entered the bridge
Car #5 arrived
                              Car #5 left the bridge
Car #5 entered the bridge
                              Car #6 arrived
Car #5 left the bridge


Test output for when there is more traffic travelling SouthBound:

Starting traffic...
Car S-B#0 arrived
Car N-A#0 arrived
Car S-A#0 arrived
0.02908 Car S-B#0 entered the bridge
0.03176 Car S-A#0 entered the bridge
1.03850 Car S-B#0 left the bridge
2.03217 Car S-A#0 left the bridge
2.03257 Car N-A#0 entered the bridge
Car S-B#1 arrived
3.03969 Car N-A#0 left the bridge
Car N-A#1 arrived
3.04007 Car S-B#1 entered the bridge
Car S-A#1 arrived
4.03691 Car S-A#1 entered the bridge
5.04578 Car S-B#1 left the bridge
Car S-B#2 arrived
7.05172 Car S-B#2 entered the bridge
8.04202 Car S-A#1 left the bridge
Car S-A#2 arrived
9.04803 Car S-A#2 entered the bridge
*/


abstract class BridgeControllerTestSkeleton(bridge: BridgeControllerSkeleton) {

    import BridgeController.Direction._

    def main(args: Array[String]) = runTest()

    def runTest() = 
    {
        println("Starting traffic...")
        val traffic = generateTraffic(NorthBound, "N-A") || generateTraffic(SouthBound, "S-A") || generateTraffic(SouthBound, "S-B")
        traffic()
    }



    private def generateTraffic(direction: Direction, streamId: String) = proc 
    {
        val random      = new scala.util.Random
        var t: String   = ""

        for(i <- 0 until 100)
        {
            println(s"Car $streamId#$i arrived")

            if(NorthBound == direction)
            {
                t = bridge.arriveNorth
            }
            else
            {
                t = bridge.arriveSouth
            }

            println(s"$t Car $streamId#$i entered the bridge")

            sleep(random.nextInt(5)*Sec)

            if(NorthBound == direction)
            {
                t = bridge.leaveSouth
            }
            else
            {
                t = bridge.leaveNorth
            }

            println(s"$t Car $streamId#$i left the bridge")
            sleep(random.nextInt(3)*Sec)
        }
    }
}



object MonitorBridgeControllerTest extends BridgeControllerTestSkeleton(new MonitorBridgeController) {}
// object SemaphoreBridgeControllerTest extends BridgeControllerTestSkeleton(new SemaphoreBridgeController) {}

