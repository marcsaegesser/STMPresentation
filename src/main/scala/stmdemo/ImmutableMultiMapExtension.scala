package stmdemo

/**
 * Given a Map[A, Set[B]] this object adds extension methods
 * ala the scala.collection.mutable.MultiMap class.
 */
object ImmutableMultiMapExtension {
  implicit class ImmutableMulti[A, B](val m: Map[A, Set[B]]) extends AnyVal {
    def addBinding(a: A, b: B): Map[A, Set[B]] =
      m updated (a, m.get(a) map { s => s + b } getOrElse { Set(b) })

    def removeBinding(a: A, b: B): Map[A, Set[B]] = {
      val set = m.get(a) map { s => s - b } getOrElse { Set.empty[B] }
      if (set.isEmpty) m - a
      else m updated (a, set)
    }
  }

  import scala.concurrent.stm._
  implicit class MutableMultiTMap[A, B](val m: TMap[A, Set[B]]) extends AnyVal {
    def addBinding(a: A, b: B)(implicit txn: InTxn): Unit = {
      m += ((a, m.get(a).map(s => s + b).getOrElse(Set(b))))
    }

    def removeBinding(a: A, b: B)(implicit txn: InTxn): Unit = {
      val set = m.get(a) map { s => s - b } getOrElse { Set.empty[B] }
      if (set.isEmpty) m.remove(a)
      else m += ((a, set))
    }
  }
}
