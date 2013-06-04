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

import com.ckkloverdos.kernie.log.SLF4JKernieLogger
import com.ckkloverdos.maybe.{Just, NoVal, Failed, Maybe}
import com.ckkloverdos.resource.{StreamResource, FileStreamResourceContext}
import javax.inject.Inject
import org.junit.Test

trait Resources {
  def getResource(path: String): Maybe[StreamResource]
  def getResourceEx(path: String): StreamResource
}

class BasicResources extends Resources with ServiceSkeleton {
  val SlashEtcResourceContext = new FileStreamResourceContext("/etc/any")

  def getResource(path: String) = SlashEtcResourceContext.getResource(path)
  def getResourceEx(path: String) =
    SlashEtcResourceContext.getResource(path) match {
      case Failed(e) ⇒ throw e
      case NoVal ⇒ throw new Exception("Not found")
      case Just(r) ⇒ r
    }
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
      SLF4JKernieLogger,
      classLoader,
      classOf[Resources] -> classOf[BasicResources],
      storeWatcher,
      rollerService
    )

    val resourcesOfRollerService = rollerService.resources
    println("resourcesOfRollerService = " + resourcesOfRollerService)
    require(resourcesOfRollerService.isInstanceOf[BasicResources]) // via classOf[Resources] -> classOf[BasicResources]

    val resourcesByAPI = kernie.serviceInstanceOfInterface(classOf[Resources])
    println("resourcesByAPI = " + resourcesByAPI)
    require(resourcesByAPI eq resourcesOfRollerService)

    val resourcesInstance = kernie.serviceInstanceOf(classOf[Resources])
    val basicResourcesInstance = kernie.serviceInstanceOf(classOf[BasicResources])
    require(resourcesInstance eq basicResourcesInstance)
  }
}