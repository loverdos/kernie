/*
 * Copyright 2011-2013 Christos KK Loverdos
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
      case ke: KernieException ⇒
        val msg = ke.getMessage
        val thisMsg = format.format(args:_*)
        throw new KernieException(ke, "%s. %s", thisMsg, msg)

      case e: Throwable ⇒
        throw new KernieException(e, format, args:_*)
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

    Check(
      !impl.isPrimitive,
      "Implementation %s is primitive", impl.getSimpleName
    )
  }

  @inline
  final def loadClass(classLoader: ClassLoader, name: String, format: String, args: Any*): Class[_] = {
    try classLoader.loadClass(name)
    catch {
      case e: Throwable ⇒
        throw new KernieException(e, format, args:_*)
    }
  }

  final def loadAPIClass(classLoader: ClassLoader, apiName: String): Class[_] =
    loadClass(
      classLoader,
      apiName,
      "Could not load API class '%s' using %s", apiName, classLoader
    )

  final def loadImplClass(classLoader: ClassLoader, implName: String): Class[_] = {
    val cls = loadClass(
      classLoader,
      implName,
      "Could not load implementation class '%s' using %s", implName, classLoader
    )

    if(cls.isInterface) {
      throw new KernieException("Implementation class '%s' is an interface", implName)
    }

    cls
  }

  final def extraIndexInfo[A](seq: collection.Seq[A], index: Int, formatter: A ⇒ String): String = {
    index match {
      case 0 ⇒
        ""
      case n if n == seq.size - 1 ⇒
        " (the last one)"
      case n ⇒
        " (after %s)".format(formatter(seq(n - 1)))
    }
  }

  @inline final def toBeInjected[A <: AnyRef]: A = null.asInstanceOf[A]

  @inline final def Var[A <: AnyRef]: A = null.asInstanceOf[A]
}
