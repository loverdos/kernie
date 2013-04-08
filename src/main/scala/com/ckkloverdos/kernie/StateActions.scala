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
import State._

/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
final case class StateActions(stateVector: AtomicReference[StateVector]) extends LifecycleInfo {
  def this(state: State) = this(new AtomicReference[StateVector](StateVector(state, state)))

  def this(stateVector: StateVector) = this(new AtomicReference[StateVector](stateVector))

  def from: State = stateVector.get().from

  def to: State = stateVector.get().to

  def setVector(from: State, to: State): Unit = stateVector.set(StateVector(from, to))

  def setVector(vector: StateVector): Unit = stateVector.set(vector)

  def setReachedState(target: State): Unit = setVector(target, target)

  def vector: StateVector = stateVector.get()

  def is(state: State): Boolean = vector is state

  def isStarting: Boolean = vector match {
    case StateVector(from, STARTED) if from != STARTED ⇒
      true
    case _ ⇒
      false
  }

  def isConfiguring: Boolean = vector match {
    case StateVector(from, CONFIGURED) if from != CONFIGURED ⇒
      true
    case _ ⇒
      false
  }

  def isStopping: Boolean = vector match {
    case StateVector(from, STOPPED) if from != STOPPED ⇒
      true
    case _ ⇒
      false
  }

  def isPausing: Boolean = vector match {
    case StateVector(from, PAUSED) if from != PAUSED ⇒
      true
    case _ ⇒
      false
  }
}
