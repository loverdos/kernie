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
