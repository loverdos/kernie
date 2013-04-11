package com.ckkloverdos.kernie
package log

import org.slf4j.LoggerFactory

/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
final object SLF4JKernieLogger extends KernieLogger {
  @transient private[this] val logger = LoggerFactory.getLogger(classOf[Kernie])

  def log(fmt: String, args: Any*) {
    if(logger.isDebugEnabled) {
      logger.debug(fmt.format(args:_*))
    }
  }
}
