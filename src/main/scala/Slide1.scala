package org.saegesser

import scala.concurrent.stm._

object Slide1 {
  private val count = Ref(0)  // count: Ref[Int]
  private val data = Ref(Map.empty[Int, String])

  def add(id: Int, name: String): Int =
    atomic { implicit txn =>
      data() = data() updated (id, name)
      count() = count() + 1
      count()
    }

  def remove(id: Int): Int =
    atomic { implicit txn =>
      data() = data() - id
      count() = count() + 1
      count()
    }
}
