package com.ckkloverdos.kernie

/**
 * The basic interface that services with lifecycle should implement.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
trait Lifecycle extends Ping {
  def configure()

  def start()

  def pause()

  def stop()
}
