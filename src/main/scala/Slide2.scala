package stmdemo

import scala.concurrent.stm._

object Slide2 {
  private val count = Ref(0)  // count: Ref[Int]
  private val data = Ref(Map.empty[Int, String])

  def add(id: Int, name: String): Int =
    atomic { implicit txn =>
      data transform (_ updated (id, name))
      count transformAndGet (_ + 1)
    }

  def remove(id: Int): Int =
    atomic { implicit txn =>
      data transform (_ - id)
      count transformAndGet (_ + 1)
    }

  private val countView = count.single
  def currentCount = countView()

  private val dataView = data.single
  def currentData = dataView()

  def getOrWait(x: Int): String = {
    atomic { implicit txn =>
      data() get x getOrElse retry
    }
  }

  def getOrWait(x: Int, timeout: Long): Option[String] = {
    atomic { implicit txn =>
      data() get x match {
        case Some(s) => Some(s)
        case None => retryFor(timeout); None
      }
    }
  }
}
