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

import com.ckkloverdos.maybe.Maybe
import com.ckkloverdos.resource.{StreamResource, FileStreamResourceContext}
import javax.inject.Inject
import org.junit.Test
import junit.framework.TestCase

trait Resources {
  def getResource(path: String): Maybe[StreamResource]
  def getResourceEx(path: String): StreamResource
}

class BasicResources extends Resources {
  val SlashEtcResourceContext = new FileStreamResourceContext("/etc/any")

  def getResource(path: String) = SlashEtcResourceContext.getResource(path)
  def getResourceEx(path: String) = SlashEtcResourceContext.getResourceEx(path)
}

class SuperResources extends Resources {
  def getResource(path: String) = throw new UnsupportedOperationException
  def getResourceEx(path: String) = throw new UnsupportedOperationException
}

class RollerService {
  @Inject var resources: Resources = _
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

class ATest {
  @Test
  def testA() {
    val classLoader = Thread.currentThread().getContextClassLoader

    val rollerService = new RollerService
    val storeWatcher = new StoreWatcher
    val resourcesBinding = Binding(classOf[Resources], classOf[BasicResources])

    val kernie = new Kernie(
      classLoader,
      classOf[Resources] -> classOf[BasicResources],
      storeWatcher,
      rollerService
    )

    val resourcesOfRollerService = rollerService.resources
    println("resourcesOfRollerService = " + resourcesOfRollerService)
    require(resourcesOfRollerService.isInstanceOf[BasicResources]) // via classOf[Resources] -> classOf[BasicResources]

    val resourcesByAPI = kernie.serviceByAPI(classOf[Resources])
    println("resourcesByAPI = " + resourcesByAPI)
    require(resourcesByAPI eq resourcesOfRollerService)
  }
}