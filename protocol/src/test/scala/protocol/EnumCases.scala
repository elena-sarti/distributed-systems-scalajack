package protocol

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

/** Test helper that lists, at compile time, the names of all the cases of an enum.
 * It lets the round-trip tests check that no message kind was forgotten:
 * adding a new message without a sample makes the coverage test fail.
 */
object EnumCases:
  inline def names[T](using m: Mirror.SumOf[T]): Set[String] =
    constValueTuple[m.MirroredElemLabels].productIterator.map(_.toString).toSet
