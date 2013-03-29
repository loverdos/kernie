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

package com.ckkloverdos.kernie.tests

import com.ckkloverdos.kernie.{Binding, Kernie}
import com.ckkloverdos.resource.{StreamResource, FileStreamResourceContext}
import javax.inject.Inject
import com.ckkloverdos.maybe.Maybe

trait Resources {
  def getResource(path: String): Maybe[StreamResource]
  def getResourceEx(path: String): StreamResource
}

class BasicResources extends Resources {
  val SlashEtcResourceContext = new FileStreamResourceContext("/etc/any")

  def getResource(path: String) = SlashEtcResourceContext.getResource(path)
  def getResourceEx(path: String) = SlashEtcResourceContext.getResourceEx(path)
}

class RollerService {
  @Inject var resources: BasicResources = _
}

class UserStore {
  @Inject var resources: BasicResources = _
}

class StoreNestedAA
class StoreNestedA {
  @Inject var nestedAA: StoreNestedAA = _
}

class RCStore {
  @Inject var nestedA: StoreNestedA = _
}
class StoreWatcher {
  @Inject var userStore: UserStore = _
  @Inject var rcStore: RCStore = _
}

object Tests {
  def main(args: Array[String]) {
    val kernie = new Kernie(
      Binding(classOf[Resources], classOf[BasicResources]),
      new StoreWatcher,
      new RollerService
    )
  }
}