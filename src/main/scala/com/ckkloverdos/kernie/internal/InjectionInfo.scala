package com.ckkloverdos.kernie.internal

import scala.collection.mutable

/**
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
final case class InjectionInfo(
    instances: mutable.LinkedHashSet[AnyRef],
    instanceByClass: mutable.LinkedHashMap[Class[_], AnyRef],
    immediateClassDependencies: mutable.LinkedHashMap[Class[_], ImmediateDependencies],
    linearizedDependencies: mutable.LinkedHashSet[Class[_]],
    implByAPI: mutable.LinkedHashMap[Class[_], Class[_]] // binding info
)
