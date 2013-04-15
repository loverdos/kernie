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
