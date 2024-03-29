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

package com.ckkloverdos.kernie.log

/**
 * A logger for Kernie itself. The implementation is provided by application code.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
trait KernieLogger {
  def debug(fmt: String, args: Any*)
  def info(fmt: String, args: Any*)
  def warn(fmt: String, args: Any*)
  def error(e: Throwable, fmt: String, args: Any*)
}
