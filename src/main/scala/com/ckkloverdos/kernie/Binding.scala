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

package com.ckkloverdos.kernie

/**
 * Binds together an interface and implementation.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
final class Binding[T] private(val api: Class[T], val impl: Class[_ <: T]) {

  override def hashCode() = api.## * 31 + impl.##

  override def equals(obj: Any) = obj match {
    case that: Binding[_] ⇒ this.api == that.api && this.impl == that.impl
    case _ ⇒ false
  }

  override def toString = "%s(%s, %s)".format(getClass.getSimpleName, api.getSimpleName, impl.getSimpleName)

  @inline final def untypedAPI: Class[_] = api

  @inline final def untypedImpl: Class[_] = impl

  @inline final def toTuple: (Class[T], Class[_ <: T]) = (api, impl)

  @inline final def toUntypedTuple: (Class[_], Class[_]) = (api, impl)
}

object Binding {
  def apply[T](api: Class[T], impl: Class[_ <: T]): Binding[T] = {
    CheckBinding(api, impl)
    new Binding[T](api, impl)
  }

  def dynamic[A, B](api: Class[A], impl: Class[B]): Binding[_] = {
    CheckBinding(api, impl) // I need exceptions early
    new Binding(api, impl.asSubclass(api))
  }
}
