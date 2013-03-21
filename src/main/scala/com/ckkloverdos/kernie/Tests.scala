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
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
trait Logger extends ServiceSkeleton[Logger] {
  def log(fmt: String, args: Any*)
}

object LoggerDef extends ServiceDefSkeleton[Logger]()

trait LoginService extends ServiceSkeleton[LoginService] {
  type Authenticated
  def login(username: String, password: String): Option[Authenticated]
}

object LoginServiceDef extends ServiceDefSkeleton[LoginService](Set(LoggerDef.id))

object Tests {
  val kernie = new KernieBuilder().
    add(LoginServiceDef).
    add(LoggerDef).
  build
}