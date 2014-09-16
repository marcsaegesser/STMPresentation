package stmdemo

import scala.language.implicitConversions

/**
  * Given a Map[A, Set[B]] this object adds extension methods
  * ala the scala.collection.mutable.MultiMap class.
  */
object ImmutableMultiMapExtension {
  class ImmutableMulti[A, B](val m: Map[A, Set[B]]) extends AnyVal {
    def addBinding(a: A, b: B): Map[A, Set[B]] =
      m updated (a, m.get(a) map { s => s + b } getOrElse { Set(b) })

    def removeBinding(a: A, b: B): Map[A, Set[B]] = {
      val set = m.get(a) map { s => s - b } getOrElse { Set.empty[B] }
      if (set.isEmpty) m - a
      else m updated (a, set)
    }
  }

  implicit def mapToMulti[A, B](m: Map[A, Set[B]]) = new ImmutableMulti(m)
}
