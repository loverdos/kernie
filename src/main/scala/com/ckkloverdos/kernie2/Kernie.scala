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
  private var _providerByServiceClass = Map[ServiceClass, Provider]()
  // The dependencies of each service
  private var _serviceClassDependencies = Map[ServiceClass, Set[ServiceClass]]()
  private var _linearizedClassDependencies = List[ServiceClass]()
  // The fields each singleton service needs to be injected
  private var _fieldsToInjectByInstance = Map[Service, List[Field]]()
  // The reflective fields (along with their respective instances) that are of the same type
  private var _fieldsOfTheSameType = Map[Class, List[(Field, Service)]]()

  private val logger = LoggerFactory.getLogger(this.getClass)

  _init(items)

//  private def _addFieldsToInject(owner: AnyRef) {
//
//    def addFieldOfType(fieldType: Class, field: Field, fieldValue: Service) {
//      _fieldsOfTheSameType.get(fieldType) match {
//        case None ⇒
//          _fieldsOfTheSameType += fieldType -> (field, fieldValue)
//
//        case Some(list) ⇒
//          _fieldsOfTheSameType += fieldType -> ((field, fieldValue) :: list)
//      }
//    }
//
//    val fieldsToInject = owner.
//      getClass.
//      getDeclaredFields.
//      filter(_isFieldToInject).
//      map(f ⇒ { f.setAccessible(true); f }).
//      toList
//
//    _fieldsToInjectByInstance += owner -> fieldsToInject
//
//    for(field ← fieldsToInject) {
//      val fieldType = field.getType
//      val fieldValue = _getOrInstantiateService(fieldType)
//      addFieldOfType(fieldType, field, fieldValue)
//    }
//  }

  def _isFieldToInject(field: Field) =
    field.getAnnotation(classOf[Inject]) ne null

  private def _discoverDependencyClasses(service: Service) {
    // Currently, the dependencies are all those declared via the
    // fields that need to be injected

    val fieldsToInject = service.
      getClass.
      getDeclaredFields.
      filter(_isFieldToInject).
      map(field ⇒ {
        field.setAccessible(true)
        logger.debug("Must inject %s::%s: %s".format(
          service.getClass.getName,
          field.getName,
          field.getType.getName))

        field
      }).
      toList


    _fieldsToInjectByInstance += service -> fieldsToInject

    val fieldClassesToInject = fieldsToInject.map(_.getType).toSet
    val serviceClass = service.getClass
    _serviceClassDependencies += serviceClass -> fieldClassesToInject
  }

  private def _registerInitialServices(services: Seq[AnyRef]) {
    for(service ← services) {
      val serviceClass = service.getClass

      if(service eq null) {
        throw new KernieException("Service is null")
      }
      if(_services.contains(service)) {
        throw new KernieException("Service %s already provided", service.getClass.getName)
      }
      _serviceByClass.get(serviceClass) match {
        case Some(otherService) if otherService ne service ⇒
          throw new KernieException("Service %s already provided by another instance", service.getClass.getName)
        case _ ⇒
      }

      logger.debug("Registering %s".format(service.getClass.getName))
      _discoverDependencyClasses(service)

      _services += service

      _serviceByClass += serviceClass -> service
    }

    logger.debug("%s Known services: %s".format(
      _services.size,
      _services.map(_.getClass.getName).mkString(", "))
    )
  }

  private def _linearizeDependencies() {
    val buffer = scala.collection.mutable.ListBuffer[ServiceClass]()
    val marked = scala.collection.mutable.Set[ServiceClass]()

    def visit(serviceClass: ServiceClass, nest: Int) {
      if(marked(serviceClass)) {
        return
      }

      marked(serviceClass) = true

      for {
        dependencies ← _serviceClassDependencies.get(serviceClass)
        dependencyClass ← dependencies
      } {
        visit(dependencyClass, nest + 1)
      }

      buffer += serviceClass
    }

    for {
      service ← _services
      serviceClass = service.getClass
    } {
      visit(serviceClass, 0)
    }

    _linearizedClassDependencies = buffer.toList

    logger.debug("%s Linearized services: %s".format(
      _linearizedClassDependencies.size,
      _linearizedClassDependencies.map(_.getName).mkString(", "))
    )
  }

//  private def _getOrInstantiateInjection(cls: ServiceClass): AnyRef = {
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

  private def _injectDependencies() {
    val instance_fields = for {
      serviceClass ← _linearizedClassDependencies
      service ← _serviceByClass.get(serviceClass)
      fields ← _fieldsToInjectByInstance.get(service)
    } yield (service, fields)

    for {
      (instance, fields) ← instance_fields
      field ← fields
    } {
      val fieldClass = field.getType
      _serviceByClass.get(fieldClass) match {
//        case None ⇒
//          // Service has not been resolved yet

        case Some(fieldValue) ⇒
          try {
            logger.debug("Injecting %s into %s: %s of %s".format(
              fieldValue, field.getName, field.getType.getName, instance
            ))
            field.set(instance, fieldValue)
          }
          catch {
            case e: Throwable ⇒
              throw new KernieException(
                "Could not inject %s: %s into %s",
                field.getName,
                field.getType.getName,
                instance
            )
          }
      }
    }
  }

  private def _init(services: Seq[AnyRef]) {
    _registerInitialServices(services)

    _linearizeDependencies()

    _injectDependencies()
  }

}

object Kernie {
  private[kernie2] type Class = Predef.Class[_]
  private[kernie2] type ServiceClass = Class
  private[kernie2] type Service = AnyRef
  private[kernie2] type Provider = AnyRef
}
