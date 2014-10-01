package stmdemo

import scala.concurrent.stm._

object Slide3 {
  private val count = Ref(0).single  // count: Ref.View[Int]
  private val data = TMap.empty[Int, String].single

  def add(id: Int, name: String): Int =
    atomic { implicit txn =>
      data += (id -> name)
      count transformAndGet (_ + 1)
    }

  def remove(id: Int): Int =
    atomic { implicit txn =>
      data -= id
      count transformAndGet (_ + 1)
    }

  def currentCount = count()

  def currentData = data.snapshot

  def getOrWait(x: Int): String =
    atomic { implicit txn =>
      data get x getOrElse retry
    }

  def getOrWait(x: Int, timeout: Long): Option[String] =
    atomic { implicit txn =>
      data get x match {
        case s @ Some(_) => s
        case None        => retryFor(timeout); None
      }
    }
}
