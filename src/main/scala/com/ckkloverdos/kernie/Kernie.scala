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

import java.util.concurrent.atomic.{AtomicReference, AtomicBoolean}


/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
class Kernie private[kernie](
    val serviceDefByID: Map[CharSequence, ServiceDef[_]],
    val serviceByID: Map[CharSequence, AnyRef],
    val idByServiceDef: Map[ServiceDef[_], CharSequence],
    val sortedServiceDefs: Seq[ServiceDef[_]],
    val dependentsOf: Map[CharSequence, Seq[ServiceDef[_]]]
) {
  private[this] val _state = new AtomicReference[State](State.STOPPED)
  private[this] val _allStarted = new AtomicBoolean(false)
  private[this] val _isStarting = new AtomicBoolean(true)

  private[this] def _start(serviceDef: ServiceDef[_]) {
    println("Starting " + serviceDef)
    val id = serviceDef.id
    val service = serviceByID.apply(id)
  }

  def state = _state.get()

  def startServices() {
    state match {
      case State.PAUSED | State.STOPPED ⇒
        try {
          for(sortedServiceDef <- sortedServiceDefs) {
            _start(sortedServiceDef)
          }

          _state.set(State.STARTED)
        }
        catch {
          case e: Throwable ⇒
            throw new KernieException("%s could not be started", classOf[Kernie].getSimpleName, e)
        }

      case state ⇒
        throw new KernieException("%s cannot be started while in state %s", classOf[Kernie].getSimpleName, state)
    }
  }

  def stopServices() {

  }

  override def toString = "%s(%s)".format(getClass.getSimpleName, sortedServiceDefs.mkString(", "))
}

