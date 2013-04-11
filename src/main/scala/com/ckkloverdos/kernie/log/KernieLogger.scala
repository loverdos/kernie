package com.ckkloverdos.kernie.log

/**
 * A logger for Kernie itself. The implementation is provided by application code.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
trait KernieLogger {
  def log(fmt: String, args: Any*)
}
