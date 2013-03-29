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

import scala.collection.mutable

/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
class KernieBuilder() {
  private var serviceDefByID = Map[CharSequence, ServiceDef[_]]()
  private var idByServiceDef = Map[ServiceDef[_], CharSequence]()
  private var dependendsOf = Map[CharSequence, List[ServiceDef[_]]]()
  private var serviceByID = Map[CharSequence, AnyRef]()

  private def addReverseDependency(serviceDef: ServiceDef[_], dependencyID: CharSequence) {
    dependendsOf.get(dependencyID) match {
      case None ⇒
        dependendsOf += dependencyID -> List(serviceDef)

      case Some(list) ⇒
        dependendsOf += dependencyID -> (serviceDef :: list)
    }
  }

  private[this] def _check(serviceDef: ServiceDef[_]) {
    require(serviceDef ne null, "serviceDef ne null")
    require(serviceDef.id ne null, "serviceDef.id ne null")
    require(serviceDef.dependencies ne null, "serviceDef.dependencies ne null")
    require(serviceDef.key ne null, "serviceDef.key ne null")
  }

  def add[T <: Service](serviceDef: ServiceDef[T]): this.type = {
    _check(serviceDef)

    try {
      val instance = serviceDef.key.keyType.erasure.newInstance().asInstanceOf[T]

      this._add(serviceDef, instance)
    }
    catch {
      case e:Throwable ⇒
        throw new KernieException("Could not construct an instance of %s", e)
    }
  }

  def add[T <: Service](serviceDef: ServiceDef[T], service: T): this.type = {
    _check(serviceDef)

    _add(serviceDef, service)
  }

  private[this] def _add[T <: Service](serviceDef: ServiceDef[T], service: T): this.type = {
    require(serviceDef.id == service.serviceDefID, "serviceDef.id == service.serviceDefID")
    require(service.state == State.STOPPED, "service.state == State.STOPPED")

    val id = serviceDef.id
    serviceDefByID.get(id) match {
      case None ⇒
        idByServiceDef.get(serviceDef) match {
          case None ⇒
            serviceDefByID += id -> serviceDef
            serviceByID    += id -> service

            for(dependency <- serviceDef.dependencies) {
              addReverseDependency(serviceDef, dependency)
            }

          case Some(existingID) ⇒
            throw new Exception(
              "ServiceDef = %s with ID = %s already exists with another ID = %s".format(
              serviceDef, id, existingID
            ))
        }

      case Some(existingServiceDef) ⇒
        throw new Exception(
          "ID = %s of %s already exists for %s".format(
          id, serviceDef, existingServiceDef
        ))
    }

    this
  }

  private[this] def sortedServiceDefs: List[ServiceDef[_]] = {
    val buffer = new mutable.ListBuffer[ServiceDef[_]]
    val marked = new mutable.HashSet[ServiceDef[_]]()

    def visit(serviceDef: ServiceDef[_], nest: Int) {
//      println((" " * nest) + "Visit " + serviceDef)
      if(marked(serviceDef)) {
        return
      }

      marked(serviceDef) = true
//      println((" " * nest) + "Mark " + serviceDef + ", all marked = " + marked)

      for(dependency <- serviceDef.dependencies) {
        visit(serviceDefByID(dependency), nest + 1)
      }

//      println((" " * nest) + "Add " + serviceDef)
      buffer += serviceDef
    }

    for(serviceDef <- serviceDefByID.valuesIterator) {
      visit(serviceDef, 0)
    }

    buffer.toList
  }

  def build: Kernie = {
    new Kernie(
      serviceDefByID,
      serviceByID,
      idByServiceDef,
      sortedServiceDefs,
      dependendsOf
    )
  }
}
