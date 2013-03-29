package com.ckkloverdos

import scala.collection.mutable
import java.lang.reflect.Field

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

  final class LinkedSet[T] private(elems: Vector[T], set: Set[T]) {
    def this(t: T) = this(Vector(t), Set(t))

    def +(t: T) = new LinkedSet(elems :+ t, set + t)
    def contains(t: T) = set.contains(t)
    def map[U](f: T ⇒ U) = elems.map(f)
  }

  final case class ImmediateDependencies(
    fieldsToInject: List[Field],
    classesToInject: List[Class[_]]
  )

  final case class InitialServiceInfo(
      services: mutable.LinkedHashSet[AnyRef],
      serviceByClass: mutable.LinkedHashMap[Class[_], AnyRef]
  )

  final case class DependencyInfo(
      serviceClasses: mutable.LinkedHashSet[Class[_]],
      immediateClassDependencies: mutable.LinkedHashMap[Class[_], ImmediateDependencies],
      linearizedDependencies: mutable.LinkedHashSet[Class[_]]
  )
}
