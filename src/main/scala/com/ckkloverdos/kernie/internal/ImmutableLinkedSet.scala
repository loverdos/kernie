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

package com.ckkloverdos.kernie.internal

/**
 * Immutable linked set with just the operations needed for `kernie`.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
final class ImmutableLinkedSet[T](elems: Vector[T], set: Set[T]) {
  def this(t: T) = this(Vector(t), Set(t))

  def +(t: T) = new ImmutableLinkedSet(elems :+ t, set + t)
  def contains(t: T) = set.contains(t)
  def map[U](f: T ⇒ U) = elems.map(f)
}
