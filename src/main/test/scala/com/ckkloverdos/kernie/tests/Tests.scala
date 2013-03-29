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
package tests

import com.ckkloverdos.kernie.ServiceSkeleton

/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
trait Logger extends ServiceSkeleton[Logger] {
  def log(fmt: String, args: Any*)
}

object Logger extends ServiceDefSkeleton[Logger]()

trait LoginService extends ServiceSkeleton[LoginService] {
  type Authenticated
  def login(username: String, password: String): Option[Authenticated]
}

object LoginService extends ServiceDefSkeleton[LoginService](Set(Logger.id))

///////////////////
trait MasterProps extends ServiceSkeleton[MasterProps]
object MasterProps extends ServiceDefSkeleton[MasterProps]

trait Aquarium extends ServiceSkeleton[Aquarium]
object Aquarium extends ServiceDefSkeleton[Aquarium](Set(MasterProps.id))

trait AkkaService extends ServiceSkeleton[AkkaService]
object AkkaService extends ServiceDefSkeleton[AkkaService](Set(Aquarium.id))

trait ChargingService extends ServiceSkeleton[ChargingService]
object ChargingService extends ServiceDefSkeleton[ChargingService](Set(Aquarium.id))

trait RESTService extends ServiceSkeleton[RESTService]
object RESTService extends ServiceDefSkeleton[RESTService](Set(MasterProps.id))

trait StoreProvider extends ServiceSkeleton[StoreProvider] {
  type Store
  def provideStore: Store
}
object StoreProvider extends ServiceDefSkeleton[StoreProvider](Set(MasterProps.id))

object Tests {
  val kernie = new KernieBuilder().
    add(LoginService).
    add(Logger).
    add(AkkaService).
    add(MasterProps).
    add(Aquarium).
    add(ChargingService).
    add(RESTService).
  build
}