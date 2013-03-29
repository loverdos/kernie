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

  @inline
  final def Check(condition: Boolean, format: String, args: Any*) {
    if(!condition) throw new KernieException(format.format(args:_*))
  }

  @inline
  final def checkBinding(api: Class[_], impl: Class[_]) {
    Check(
      impl ne null,
      "Implementation for %s is null", api.getName
    )

    Check(
      api ne null,
      "Interface for %s is null", impl.getName
    )

    Check(
      !impl.isInterface,
      "Implementation %s for %s is an interface", impl.getName, api.getName
    )

    Check(
      api.isInterface,
      "API %s for %s is not an interface", api.getName, impl.getName
    )

    Check(
      api.isAssignableFrom(impl),
      "Implementation %s is incompatible with %s", impl.getName, api.getName
    )
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
      instances: mutable.LinkedHashSet[AnyRef],
      instanceByClass: mutable.LinkedHashMap[Class[_], AnyRef]
  )

  final case class DependencyInfo(
      serviceClasses: mutable.LinkedHashSet[Class[_]],
      immediateClassDependencies: mutable.LinkedHashMap[Class[_], ImmediateDependencies],
      linearizedDependencies: mutable.LinkedHashSet[Class[_]]
  )
}
