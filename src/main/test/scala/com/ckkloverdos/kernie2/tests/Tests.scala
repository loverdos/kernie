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

package com.ckkloverdos.kernie2.tests

import com.ckkloverdos.kernie2.Kernie
import com.ckkloverdos.resource.FileStreamResourceContext
import javax.inject.Inject

class BasicResources {
  val SlashEtcResourceContext = new FileStreamResourceContext("/etc/any")

  def getResource(path: String) = SlashEtcResourceContext.getResource(path)
  def getResourceEx(path: String) = SlashEtcResourceContext.getResourceEx(path)
}

class AkkaService {
  @Inject private var resources: BasicResources = _
}

object Tests {
  def main(args: Array[String]) {
    val kernie = new Kernie(
      new AkkaService,
      new BasicResources
    )
  }
}