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
  // The service instance for a specific service class/type
  private val _serviceByClass = mutable.LinkedHashMap[Class[_], AnyRef]()

  // The set of singleton services
  private val _services = mutable.LinkedHashSet[AnyRef]()
  // The service provider of each service (by the service class)
  private var _providerByClass = Map[Class[_], AnyRef]()
  // The dependencies of each service
  private var _serviceClassDependencies = Map[Class[_], Set[Class[_]]]()
  private var _linearizedClassDependencies = List[Class[_]]()
  // The fields each singleton service needs to be injected
  private var _fieldsToInjectByInstance = Map[AnyRef, List[Field]]()
  // The reflective fields (along with their respective instances) that are of the same type
  private var _fieldsOfTheSameType = Map[Class[_], List[(Field, AnyRef)]]()

  private val logger = LoggerFactory.getLogger(this.getClass)

  _init(items)

  def _isFieldToInject(field: Field) =
    field.getAnnotation(classOf[Inject]) ne null

  // Updates _services && _serviceByClass
  private def _initialServiceInfoOf(services: Seq[AnyRef]): InitialServiceInfo = {
    val services = new mutable.LinkedHashSet[AnyRef]
    val serviceByClass = new mutable.LinkedHashMap[Class[_], AnyRef]

    for(service ← services) {
      val serviceClass = service.getClass

      if(service eq null) {
        throw new KernieException("Service is null")
      }
      if(services.contains(service)) {
        throw new KernieException("Service %s already provided", service.getClass.getSimpleName)
      }
      serviceByClass.get(serviceClass) match {
        case Some(otherService) if otherService ne service ⇒
          throw new KernieException("Service %s already provided by another instance", service.getClass.getSimpleName)
        case _ ⇒
      }

      services += service

      serviceByClass += serviceClass -> service
    }

    InitialServiceInfo(services, serviceByClass)
  }

  private def _injectionFieldsOf(cls: Class[_], nest: Int): List[Field] = {
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
  private def _immediateDependenciesOf(cls: Class[_], nest: Int): ImmediateDependencies = {
    val fieldsToInject = _injectionFieldsOf(cls, nest)
    val classesToInject = fieldsToInject.map(_.getType)/*: List[Class[_]]*/

    ImmediateDependencies(
      fieldsToInject,
      classesToInject
    )
  }

  // The initialSet of known classes bootstraps the dependency graph
  private def _computeDependencyInfo(serviceClasses: scala.collection.Set[Class[_]]): DependencyInfo = {
    val linearizedDeps = mutable.LinkedHashSet[Class[_]]()
    val explored = mutable.LinkedHashSet[Class[_]]()
    val allServiceClasses = new mutable.LinkedHashSet[Class[_]] ++ serviceClasses
    val allImmediateClassDependencies = new mutable.LinkedHashMap[Class[_], ImmediateDependencies]

    def explore(
        cls: Class[_],
        path: LinkedSet[Class[_]],
        deps: mutable.LinkedHashSet[Class[_]],
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
      allImmediateClassDependencies += cls -> immediateDeps

      for(immediateClassDep ← immediateDeps.classesToInject) {
        if(path.contains(immediateClassDep)) {
          throw new KernieException(
            "Circular dependency in path %s",
            (path + immediateClassDep).map(_.getSimpleName).mkString(" -> ")
          )
        }

        explore(
          immediateClassDep,
          path + immediateClassDep,
          deps,
          nest + 1
        )
      }

      deps += cls
      explored += cls
      allServiceClasses += cls
    }

    for {
      cls ← serviceClasses
    } {
      explore(cls, new LinkedSet(cls), linearizedDeps, 0)
    }

    DependencyInfo(
      allServiceClasses,
      allImmediateClassDependencies,
      linearizedDeps
    )
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
    def logServices(format: String, serviceClasses: collection.Set[Class[_]]) {
      logger.debug(format.format(serviceClasses.map(_.getSimpleName).mkString(", ")))
    }

    val initialServiceInfo = _initialServiceInfoOf(services)
    _services ++= initialServiceInfo.services
    _serviceByClass ++= initialServiceInfo.serviceByClass

    val initialServiceClasses = initialServiceInfo.serviceByClass.keySet
    if(logger.isDebugEnabled()) {
      logServices("Initial services: %s", initialServiceClasses)
    }

    val serviceClasses = mutable.LinkedHashSet[Class[_]](services.map(_.getClass):_*)
    val dependencyInfo = _computeDependencyInfo(serviceClasses)

    if(logger.isDebugEnabled) {
      logServices("New services: %s", dependencyInfo.serviceClasses -- initialServiceClasses)
    }

    logger.debug("Linearized dependencies: %s".format(
      dependencyInfo.
        linearizedDependencies.
        map(_.getSimpleName).
        zipWithIndex.
        mkString(", "))
    )

    //    _linearizeDependencies()

//    _injectDependencies()
  }

}
