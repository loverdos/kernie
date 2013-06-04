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
package log

import org.slf4j.LoggerFactory

/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
final object SLF4JKernieLogger extends KernieLogger {
  @transient private[this] val logger = LoggerFactory.getLogger(classOf[Kernie])

  def debug(fmt: String, args: Any*) {
    if(logger.isDebugEnabled) {
      logger.debug(fmt.format(args:_*))
    }
  }

  def info(fmt: String, args: Any*) {
    if(logger.isInfoEnabled) {
      logger.info(fmt.format(args:_*))
    }
  }

  def warn(fmt: String, args: Any*) {
    if(logger.isWarnEnabled) {
      logger.warn(fmt.format(args:_*))
    }
  }

  def error(e: Throwable, fmt: String, args: Any*) {
    if(logger.isErrorEnabled) {
      logger.error(fmt.format(args:_*), e)
    }
  }
}
