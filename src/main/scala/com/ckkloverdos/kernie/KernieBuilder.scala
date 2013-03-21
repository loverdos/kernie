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
  private var serviceDefByID: Map[CharSequence, ServiceDef[_]] = Map()
  private var idByServiceDef: Map[ServiceDef[_], CharSequence] = Map()
  private var dependendsOf: Map[CharSequence, List[ServiceDef[_]]] = Map()

  private def addReverseDependency(serviceDef: ServiceDef[_], dependencyID: CharSequence) {
    dependendsOf.get(dependencyID) match {
      case None ⇒
        dependendsOf += dependencyID -> List(serviceDef)

      case Some(list) ⇒
        dependendsOf += dependencyID -> (serviceDef :: list)
    }
  }

  def add[T](serviceDef: ServiceDef[T]): this.type = {
    val id = serviceDef.id
    serviceDefByID.get(id) match {
      case None ⇒
        idByServiceDef.get(serviceDef) match {
          case None ⇒
            serviceDefByID += id -> serviceDef
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

  private def sortedServiceDefs: List[ServiceDef[_]] = {
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
      idByServiceDef,
      sortedServiceDefs,
      dependendsOf
    )
  }
}
