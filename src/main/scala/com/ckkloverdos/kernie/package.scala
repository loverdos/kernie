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

  final def CheckBinding(api: Class[_], impl: Class[_]) {
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
}
