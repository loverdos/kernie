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
trait ServiceSkeleton extends Lifecycle with LifecycleInfo {
  private[this] val stateActions = new StateActions(State.STOPPED)

  def configure() {}

  def start() {}

  def pause() {}

  def stop() {}

  final def is(state: State) = stateActions.is(state)

  final def isStopped = stateActions.isStopped

  final def isConfigured = stateActions.isConfigured

  final def isStarted = stateActions.isStarted

  final def isPaused = stateActions.isPaused

  final def isStopping = stateActions.isStopping

  final def isConfiguring = stateActions.isConfiguring

  final def isPausing = stateActions.isPausing

  final def isStarting = stateActions.isStarting
}
