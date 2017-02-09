import io.threadcso._
import io.threadcso.semaphore._


/* Test output from MonitorModThreeCoordinatorTest: 

0.02375 Agent #3 entering
0.02375 Agent #1 entering
0.02953 Agent #3 entered (sum 3)
0.02953 Agent #1 entered (sum 4)
1.02805 Agent #3 quit (sum 1)
2.03190 Agent #4 entering
2.03195 Agent #1 quit (sum 0)
2.03249 Agent #4 entered (sum 4)
2.03292 Agent #5 entering
3.03316 Agent #10 entering
6.03535 Agent #4 quit (sum 0)
6.03544 Agent #5 entered (sum 5)
7.04086 Agent #10 entered (sum 10)
7.04086 Agent #5 quit (sum 0)
7.04146 Agent #14 entering
7.04164 Agent #10 quit (sum 0)
7.04190 Agent #14 entered (sum 14)
7.04209 Agent #14 quit (sum 0)

Test output from SemaphoreModThreeCoordinatorTest:

0.03122 Agent #6 entering
0.03122 Agent #7 entering
0.03937 Agent #7 entered (sum 13)
0.03937 Agent #6 entered (sum 6)
0.03989 Agent #0 entering
2.04404 Agent #8 entering
4.04414 Agent #6 quit (sum 7)
5.05010 Agent #0 entered (sum 0)
5.05014 Agent #8 entered (sum 8)
5.05000 Agent #7 quit (sum 0)
6.05554 Agent #15 entering
8.05265 Agent #8 quit (sum 0)
8.05246 Agent #7 entering
8.05292 Agent #15 entered (sum 15)
8.05342 Agent #7 entered (sum 22)
8.05393 Agent #3 entering
9.05847 Agent #15 quit (sum 7)
11.05418 Agent #11 entering
12.06054 Agent #0 quit (sum 7)
13.06634 Agent #3 entered (sum 3)
13.06628 Agent #7 quit (sum 0)
*/


object MonitorModThreeCoordinatorTest extends ModCoordinatorTestSkeleton(new MonitorModCoordinator(3), 16) {}
object SemaphoreModThreeCoordinatorTest extends ModCoordinatorTestSkeleton(new SemaphoreModCoordinator(3), 16) {}


class MonitorModCoordinator(M: Int) extends ModCoordinatorSkeleton(M)
{
    private [this] val monitor                      = new Monitor
    private [this] val sumOfAgentsIdsIsDivisibleByM = monitor.newCondition

    override def enter(agentId: Int): Int  = monitor withLock { super.enter(agentId) }
    override def exit(agentId: Int): Int   = monitor withLock { super.exit(agentId)  }

    protected def waitForEnterPermission(): Unit = sumOfAgentsIdsIsDivisibleByM.await()
    protected def signalEnterPermission(): Unit  = sumOfAgentsIdsIsDivisibleByM.signal()
}



class SemaphoreModCoordinator(M: Int) extends ModCoordinatorSkeleton(M)
{
    private [this] var fakeMutex                    = new BooleanSemaphore(available = true)
    private [this] val sumOfAgentsIdsIsDivisibleByM = new BooleanSemaphore(available = false)

    override protected def registerAgent(agentId: Int): Unit   = { lockMutex(); super.registerAgent(agentId); unlockMutex() }
    override protected def deregisterAgent(agentId: Int): Unit = { lockMutex(); super.deregisterAgent(agentId); unlockMutex() }
    override protected def agentsCanEnter(): Boolean           = { lockMutex(); val r = super.agentsCanEnter(); unlockMutex(); return r }

    protected def waitForEnterPermission(): Unit = sumOfAgentsIdsIsDivisibleByM.acquire()
    protected def signalEnterPermission(): Unit  = sumOfAgentsIdsIsDivisibleByM.release()

    private def lockMutex(): Unit   = fakeMutex.acquire()
    private def unlockMutex(): Unit = fakeMutex.release()
}



abstract class ModCoordinatorSkeleton(M: Int)
{
    protected def waitForEnterPermission(): Unit
    protected def signalEnterPermission(): Unit

    protected [this] var sumOfAgentIds: Int = 0


    def enter(agentId: Int): Int = 
    {
        waitUntilAgentCanEnter()
        registerAgent(agentId)
        signalIfAgentsCanEnter()

        return sumOfAgentIds
    }



    def exit(agentId: Int): Int =
    {
        deregisterAgent(agentId)
        signalIfAgentsCanEnter()

        return sumOfAgentIds
    }



    protected def registerAgent(agentId: Int): Unit =
    {
        sumOfAgentIds += agentId
    }



    protected def deregisterAgent(agentId: Int): Unit =
    {
        sumOfAgentIds -= agentId
    }



    protected def agentsCanEnter(): Boolean = 
    {
        return (0 == sumOfAgentIds % M)
    }



    protected def waitUntilAgentCanEnter(): Unit =
    {
        while(!agentsCanEnter())
        {
            waitForEnterPermission()
            
        }
    }



    protected def signalIfAgentsCanEnter(): Unit = 
    {
        if(agentsCanEnter())
        {
            signalEnterPermission()
        }
    }
}



abstract class ModCoordinatorTestSkeleton(coordinator: ModCoordinatorSkeleton, agentPoolSize: Int) {

    private [this] val availableAgents  = scala.collection.mutable.Set[Int]()
    private [this] val deployedAgents   = scala.collection.mutable.Set[Int]()
    private [this] val random           = new scala.util.Random
    private [this] val sleepTime: Int   = 5

    private val start           = nanoTime
    private def now             = (nanoTime-start)/1E9
    private def elapsedTime()   = (f"$now%6.5f") 


    def main(args: Array[String]) = runTest()



    def runTest(): Unit = 
    {
        populateAgentPools()
        val agentTrader = deployAgents || deployAgents || withdrawAgents
        agentTrader()
    }



    private def deployAgents() = proc 
    {
        while(true)
        {
            if(0 < availableAgents.size)
            {
                deployAgent(getAvailableAgentAtRandom())
            }
            sleep(random.nextInt(sleepTime)*Sec)
        }
    }



    private def withdrawAgents() = proc
    {
        while(true)
        {
            sleep(random.nextInt(sleepTime)*Sec)
            if(0 < deployedAgents.size)
            {
                withdrawAgent(getDeployedAgentAtRandom())
            }
        }
    }



    private def deployAgent(agentId: Int): Unit = 
    {
        printWithTimestamp(s"Agent #$agentId entering")

        deregsiterAvailableAgent(agentId)
        val s = coordinator.enter(agentId)
        registerDeployedAgent(agentId)

        printWithTimestamp(s"Agent #$agentId entered (sum $s)")
    }


 
    private def withdrawAgent(agentId: Int): Unit = 
    {
        deregisterDeployedAgent(agentId)
        val s = coordinator.exit(agentId)
        registerAvailableAgent(agentId)

        printWithTimestamp(s"Agent #$agentId quit (sum $s)")
    }



    private def getAvailableAgentAtRandom(): Int = synchronized
    {
        val n = random.nextInt(availableAgents.size)
        return availableAgents.iterator.drop(n).next
    }



    private def getDeployedAgentAtRandom(): Int = synchronized
    {
        val n = random.nextInt(deployedAgents.size)
        return deployedAgents.iterator.drop(n).next
    }



    private def populateAgentPools(): Unit =
    {
        availableAgents.clear()
        deployedAgents.clear()

        for(i <- 0 until agentPoolSize)
        {
            availableAgents += i
        }
    }

    private def printWithTimestamp(s: String): Unit =
    {
        println(elapsedTime() + " " + s)
    }

    private def registerAvailableAgent(agentId: Int): Unit    = synchronized { availableAgents += agentId }
    private def deregsiterAvailableAgent(agentId: Int): Unit  = synchronized { availableAgents -= agentId }
    private def registerDeployedAgent(agentId: Int): Unit     = synchronized { deployedAgents += agentId }
    private def deregisterDeployedAgent(agentId: Int): Unit   = synchronized { deployedAgents -= agentId }
}
