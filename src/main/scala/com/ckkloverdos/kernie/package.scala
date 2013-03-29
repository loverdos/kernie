package com.ckkloverdos

/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
package object kernie {
  final def Catch[A](f: ⇒ A)(format: String, args: Any*): A = {
    try f
    catch {
      case e: KernieException ⇒
        throw e

      case e: Throwable ⇒
        throw new KernieException(format, args:_*)
    }
  }

  private[kernie]
  final class LinkedSet[T] private(elems: Vector[T], set: Set[T]) {
    def this(t: T) = this(Vector(t), Set(t))

    def +(t: T) = new LinkedSet(elems :+ t, set + t)
    def contains(t: T) = set.contains(t)
    def map[U](f: T ⇒ U) = elems.map(f)
  }
}
