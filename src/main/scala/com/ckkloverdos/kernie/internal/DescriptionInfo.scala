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
package internal

/**
 * A processed view of the descriptions given in the [[com.ckkloverdos.kernie.Kernie]] constructor.
 *
 * @param implClasses Implementation classes
 * @param bindings Bindings. See [[com.ckkloverdos.kernie.Binding]]
 * @param instances Service instances
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
case class DescriptionInfo(
  implClasses: collection.Seq[Class[_]],
  bindings: collection.Seq[Binding[_]],
  instances: collection.Seq[AnyRef]
)
