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

  private def _initialServiceInfoOf(initial: Seq[AnyRef]): InitialServiceInfo = {
    val foundServices = new mutable.LinkedHashSet[AnyRef]
    val foundServiceByClass = new mutable.LinkedHashMap[Class[_], AnyRef]

    for((service, index) ← initial.zipWithIndex) {
      if(service eq null) {
        val extraInfo = index match {
          case 0 ⇒
            ""
          case n if n == initial.size - 1 ⇒
            " (the last one)"
          case n ⇒
            " (after %s)".format(initial(n - 1).getClass.getSimpleName)
        }

        throw new KernieException("Service[%s]%s is null", index, extraInfo)
      }

      if(foundServices.contains(service)) {
        throw new KernieException("Service %s already provided", service.getClass.getSimpleName)
      }

      val serviceClass = service.getClass

      foundServiceByClass.get(serviceClass) match {
        case Some(otherService) if otherService ne service ⇒
          throw new KernieException("Service %s already provided by another instance", service.getClass.getSimpleName)

        case _ ⇒
      }

      foundServices += service

      foundServiceByClass += serviceClass -> service
    }

    InitialServiceInfo(foundServices, foundServiceByClass)
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

  private def _getOrCreateInstance(
      cls: Class[_],
      instances: mutable.LinkedHashSet[AnyRef],
      instanceByClass: mutable.LinkedHashMap[Class[_], AnyRef],
      format: String,
      args: Any*
  ): AnyRef = {
    instanceByClass.get(cls) match {
      case Some(instance) ⇒
        instance

      case None ⇒
        val instance = Catch(cls.newInstance().asInstanceOf[AnyRef])(format, args:_*)
        instances += instance
        instanceByClass += cls -> instance
        instance
    }
  }

  private def _injectDependencies(
    initialServiceInfo: InitialServiceInfo,
    dependencyInfo: DependencyInfo
  ) {
    val instances = initialServiceInfo.instances
    val instanceByClass = initialServiceInfo.instanceByClass
    val serviceClasses = dependencyInfo.serviceClasses
    val immediateClassDependencies = dependencyInfo.immediateClassDependencies
    val linearizedDependencies = dependencyInfo.linearizedDependencies

    for {
      cls ← linearizedDependencies
      clsDeps ← immediateClassDependencies.get(cls)
      field ← clsDeps.fieldsToInject
      fieldType = field.getType
    } {
      logger.debug("Injecting [%s:] %s into %s".format(field.getName, fieldType.getSimpleName, cls.getSimpleName))

      val clsInstance = _getOrCreateInstance(
        cls,
        instances,
        instanceByClass,
        "Could not create instance of owner service %s",
        cls.getSimpleName
      )

      val fieldValue = _getOrCreateInstance(
        fieldType,
        instances,
        instanceByClass,
        "Could not create instance of injected service %s",
        fieldType.getSimpleName
      )

      Catch {
        field.set(clsInstance, fieldValue)
      }("Could not set field [%s:] %s of %s", field.getName, fieldType.getSimpleName, cls.getSimpleName)
    }
  }

  private def _init(services: Seq[AnyRef]) {
    def logServices(format: String, serviceClasses: collection.Set[Class[_]]) {
      logger.debug(format.format(serviceClasses.map(_.getSimpleName).mkString(", ")))
    }

    //////////////////////
    val initialServiceInfo = _initialServiceInfoOf(services)
    _services ++= initialServiceInfo.instances
    _serviceByClass ++= initialServiceInfo.instanceByClass

    val initialServiceClasses = new mutable.LinkedHashSet[Class[_]] ++ initialServiceInfo.instanceByClass.keys
    if(logger.isDebugEnabled()) {
      logServices("Initial: %s", initialServiceClasses)
    }

    //////////////////////
    val dependencyInfo = _computeDependencyInfo(initialServiceClasses)

    if(logger.isDebugEnabled && dependencyInfo.serviceClasses.size != initialServiceClasses.size) {
      val newServiceClasses = dependencyInfo.serviceClasses -- initialServiceClasses
      logServices("New: %s", newServiceClasses)
    }

    logger.debug("Linearized dependencies: %s".format(
      dependencyInfo.
        linearizedDependencies.
        map(_.getSimpleName).
        zipWithIndex.map{case (a, b) ⇒ (b, a)}.
        mkString(", "))
    )

    _injectDependencies(initialServiceInfo, dependencyInfo)
  }

}
