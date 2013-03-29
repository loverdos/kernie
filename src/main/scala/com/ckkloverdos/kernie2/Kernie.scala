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

package com.ckkloverdos.kernie2

import java.lang.reflect.Field
import javax.inject.Inject
import org.slf4j.LoggerFactory
import scala.collection.mutable


/**
 * All services must be singletons by design.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
class Kernie(items: AnyRef*) {
  import Kernie._

  private var _providers = Set[Provider]()
  private var _servicesByProvider = Map[Provider, Set[Service]]()
  // The service instance for a specific service class/type
  private var _serviceByClass = Map[Class, Service]()
  // The set of singleton services
  private var _services = Set[Service]()
  // The service provider of each service (by the service class)
  private var _providerByClass = Map[Class, Provider]()
  // The dependencies of each service
  private var _serviceClassDependencies = Map[Class, Set[Class]]()
  private var _linearizedClassDependencies = List[Class]()
  // The fields each singleton service needs to be injected
  private var _fieldsToInjectByInstance = Map[Service, List[Field]]()
  // The reflective fields (along with their respective instances) that are of the same type
  private var _fieldsOfTheSameType = Map[Class, List[(Field, Service)]]()

  private val logger = LoggerFactory.getLogger(this.getClass)

  _init(items)

  def _isFieldToInject(field: Field) =
    field.getAnnotation(classOf[Inject]) ne null

  // Updates _services && _serviceByClass
  private def _registerInitialServices(services: Seq[AnyRef]) {
    for(service ← services) {
      val serviceClass = service.getClass

      if(service eq null) {
        throw new KernieException("Service is null")
      }
      if(_services.contains(service)) {
        throw new KernieException("Service %s already provided", service.getClass.getSimpleName)
      }
      _serviceByClass.get(serviceClass) match {
        case Some(otherService) if otherService ne service ⇒
          throw new KernieException("Service %s already provided by another instance", service.getClass.getSimpleName)
        case _ ⇒
      }

      logger.debug("Registering initial service class %s".format(service.getClass.getSimpleName))

      _services += service

      _serviceByClass += serviceClass -> service
    }
  }

  private def _injectionFieldsOf(cls: Class, nest: Int): List[Field] = {
    Catch {
      cls.
        getDeclaredFields.
        filter(_isFieldToInject).
        map(field ⇒ {
          field.setAccessible(true)
          logger.debug("%sMust inject [%s:] %s into %s".format(
            "  " * nest,
            field.getName,
            field.getType.getSimpleName,
            cls.getSimpleName
          ))

          field
        }).
        toList
    }("While discovering fields to inject for %s", cls.getSimpleName)
  }

  // Currently only injected fields contribute to dependencies
  private def _immediateDependenciesOf(cls: Class, nest: Int): List[Class] = {
    val fieldsToInject = _injectionFieldsOf(cls, nest)
    val classesToInject = fieldsToInject.map(_.getType)
    classesToInject
  }

  // The initialSet of known classes bootstraps the dependency graph
  private def _computeDependencyGraph(initialSet: Set[Class]): List[Class] = {
    val linearizedDeps = mutable.ListBuffer[Class]()
    val explored = mutable.LinkedHashSet[Class]()

    def explore(
        cls: Class,
        path: LinkedSet[Class],
        allDeps: mutable.ListBuffer[Class],
        nest: Int
    ) {
      if(explored.contains(cls)) {
        logger.debug("%sAlready explored %s".format("  " * nest, cls.getSimpleName))
        return
      }

      logger.debug("%sExploring %s, path is %s".format(
        "  " * nest,
        cls.getSimpleName,
        path.map(_.getSimpleName).mkString(" -> "))
      )

      val immediateDeps = _immediateDependenciesOf(cls, nest + 1)
      for(immediateDep ← immediateDeps) {
        if(path.contains(immediateDep)) {
          throw new KernieException(
            "Circular dependency in path %s",
            (path + immediateDep).map(_.getSimpleName).mkString(" -> ")
          )
        }

        explore(
          immediateDep,
          path + immediateDep,
          allDeps,
          nest + 1
        )
      }

      allDeps += cls
      explored += cls
    }

    for {
      cls ← initialSet
    } {
      explore(cls, new LinkedSet(cls), linearizedDeps, 0)
    }

    val list = linearizedDeps.toList
    logger.debug("Linearized dependencies: %s".format(
      list.
        map(_.getSimpleName).
        zipWithIndex.
        mkString(", "))
    )
    list
  }

//  private def _getOrInstantiateInjection(cls: Class): AnyRef = {
//    _serviceByClass.get(cls) match {
//      case Some(service) ⇒
//        service
//
//      case None ⇒
//        val service = cls.newInstance().asInstanceOf[AnyRef]
//        _addService(service)
//        service
//    }
//  }

//  private def _injectDependencies() {
//    val instance_fields = for {
//      serviceClass ← _linearizedClassDependencies
//      service ← _serviceByClass.get(serviceClass)
//      fields ← _fieldsToInjectByInstance.get(service)
//    } yield (service, fields)
//
//    for {
//      (instance, fields) ← instance_fields
//      field ← fields
//    } {
//      val fieldClass = field.getType
//      _serviceByClass.get(fieldClass) match {
////        case None ⇒
////          // Service has not been resolved yet
//
//        case Some(fieldValue) ⇒
//          try {
//            logger.debug("Injecting %s into %s: %s of %s".format(
//              fieldValue, field.getSimpleName, field.getType.getSimpleName, instance
//            ))
//            field.set(instance, fieldValue)
//          }
//          catch {
//            case e: Throwable ⇒
//              throw new KernieException(
//                "Could not inject %s: %s into %s",
//                field.getSimpleName,
//                field.getType.getSimpleName,
//                instance
//            )
//          }
//      }
//    }
//  }

  private def _init(services: Seq[AnyRef]) {
    _registerInitialServices(services)

    val serviceClasses = services.map(_.getClass).toSet[Class]
    _computeDependencyGraph(serviceClasses)

//    _linearizeDependencies()

//    _injectDependencies()
  }

}

object Kernie {
  private[kernie2] type Class = Predef.Class[_]
  private[kernie2] type Service = AnyRef
  private[kernie2] type Provider = AnyRef
}
