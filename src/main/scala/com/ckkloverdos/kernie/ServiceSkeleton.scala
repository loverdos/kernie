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

import java.util.concurrent.atomic.AtomicReference
import com.ckkloverdos.kernie.event.{ServiceEventHandler, ServiceEvent, DependencyStateChange, StateChange, SystemServiceEvent}

/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
trait ServiceSkeleton[T <: Service] extends Service with ServiceEventHandler { self: T ⇒
  protected val _state = new AtomicReference[State](State.STOPPED)

  protected val _kernie = new AtomicReference[Kernie](null)

  protected val _serviceDef = new AtomicReference[ServiceDef[T]](null)

  protected val _eventHandler = new AtomicReference[ServiceEventHandler](this)

  protected def _setEventHandler(handler: ServiceEventHandler) {
    state match {
      case State.STOPPED ⇒
        _eventHandler.set(handler)

      case state ⇒
        throw new KernieException("Cannot change lifecycle of %s while in state %s", this, state)
    }
  }

  private[kernie] def _setKernie(kernie: Kernie) {
    _kernie.compareAndSet(null, kernie)
  }

  private[kernie] def _setServiceDef(serviceDef: ServiceDef[T]) {
    _serviceDef.compareAndSet(null, serviceDef)
  }

  private def _onSystemServiceEvent(event: SystemServiceEvent) {
    event match {
      case event @ StateChange(state) ⇒
        _state.set(state)
        if(eventHandler eq this) {
          onStateChanged(event)
        }
        else {
          eventHandler.onStateChanged(event)
        }

      case event @ DependencyStateChange(_, _) ⇒
        if(isSelfHandler) {
          onDependencyStateChanged(event)
        }
        else {
          eventHandler.onDependencyStateChanged(event)
        }
    }
  }

  private def isSelfHandler = eventHandler eq this

  // This is the main method called by kernie
  private[kernie] def _onServiceEvent(event: ServiceEvent) {
    if(event.isSystemEvent) {
      _onSystemServiceEvent(event.asInstanceOf[SystemServiceEvent])
    }
    else {
      if(eventHandler eq this) {
        onAppServiceEvent(event)
      }
      else {
        eventHandler.onAppServiceEvent(event)
      }
    }
  }

  protected def eventHandler = _eventHandler.get()

  protected def serviceDef = _serviceDef.get() match {
    case null ⇒
      throw new KernieException("Unknown %s for %s", classOf[ServiceDef[_]].getSimpleName, this)

    case serviceDef ⇒
      serviceDef
  }

  def state = _state.get()

  def serviceDefID = serviceDef.id

  def onStateChanged(event: StateChange) {
  }

  def onAppServiceEvent(event: ServiceEvent) {
  }

  def onDependencyStateChanged(event: DependencyStateChange) {
  }
}
