package stmdemo

import scala.language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._

import ExecutionContext.Implicits.global

object Main {
  def main(args: Array[String]) = {
    val numMessages = 10000L
    val repeatCount = 1000
    val numClients = 30
    val numTests = 10
    val dropCount = 2

    val routerSTM = new STMRouter()
    val resultsSTM = runTestSeries(routerSTM, numMessages, repeatCount, numClients, numTests, dropCount)
    println(s"STM:  Average msgs/msec=${resultsSTM.throughput}")

    val routerTMap = new TMapRouter()
    val resultsTMap = runTestSeries(routerTMap, numMessages, repeatCount, numClients, numTests, dropCount)
    println(s"TMap: Average msgs/msec=${resultsTMap.throughput}")

    val routerSync = new SyncRouter()
    val resultsSync = runTestSeries(routerSync, numMessages, repeatCount, numClients, numTests, dropCount)
    println(s"Sync: Average msgs/msec=${resultsSync.throughput}")
  }

  def elapsed[T](block: => T): (Long, T) = {
    val startTime = System.currentTimeMillis
    val result = block
    val endTime = System.currentTimeMillis
    (endTime - startTime, result)
  }

  case class TestResults(messageCount: Long, elapsedTime: Long, throughput: Long) {
    def +(other: TestResults) = TestResults(messageCount + other.messageCount, elapsedTime + other.elapsedTime, throughput + other.throughput)
  }

  def runTestSeries(router: Router, numMessages: Long, repeatCount: Int, numClients: Int, numTests: Int, dropCount: Int): TestResults = {
    def runSingleTest: TestResults = {
      val dummyConnection = new Connection() { def sendMessage(m: Message) = {}; def close() = {} }
      val clients =
        for (i <- 0 until numClients) yield new TestClient(i, i.toString, dummyConnection, router, numMessages, repeatCount)

      val (e, r) = elapsed {
        val clientFutures = Future.sequence(clients map { _.start })
        val result = Await.result(clientFutures, 60 seconds)
        result
      }


      val expectedMsgCount = numMessages * repeatCount
      if (r.exists(_ != expectedMsgCount)) throw new Exception("Did not receive all messages")

      val totalMsgCount = expectedMsgCount * numClients
      val throughput = totalMsgCount / e
      println(s"runSingleTest: elapsed=$e, throughput=$throughput")
      TestResults(totalMsgCount, e, throughput)
    }

    val testResults = for (i <- 0 until numTests) yield runSingleTest

    val sums = testResults.drop(dropCount).foldLeft(TestResults(0, 0, 0))((a: TestResults, r: TestResults) => a + r)
    val countedTests = numTests - dropCount
    TestResults(sums.messageCount / countedTests, sums.elapsedTime / countedTests, sums.throughput / countedTests)
  }
}

class TestClient(val id: ClientId, val name: String, val connection: Connection, router: Router, numMessages: Long, repeatCount: Int)
  extends Client { self =>

  var msgCount = 0

  def queueMessage(message: Message) = {
    msgCount += 1
  }

  def disconnect = {}

  def start: Future[Int] = {
    val p = Promise[Int]()

    val t = new Thread(new TestRunner(p))
    t.start()

    p.future
  }

  class TestRunner(p: Promise[Int]) extends Runnable {
    def runTest(remaining: Int): Unit =
      if (remaining > 0) {
        router.addClient(self)
        var i = 0
        while (i < numMessages) {
          router.route(Message(id, id, "Echo"))
          i += 1
        }
        router.removeClient(self)
        runTest(remaining - 1)
      }

    def run(): Unit = {
      runTest(repeatCount)
      val result = p.success(msgCount)
    }
  }
}
