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

import com.ckkloverdos.key.TKeyOnly

/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
class ServiceDefSkeleton[T: Manifest](val dependencies: Set[CharSequence]=Set()) extends ServiceDef[T] {
  final val key = new TKeyOnly[T](classManifest[T].erasure.getName)

  /**
   * The fully qualified name or UUID of the service defined by this instance.
   */
  final val id = classManifest[T].erasure.getName

  override def toString = "%s(%s)".format(getClass.getSimpleName.stripSuffix("$"), id)
}