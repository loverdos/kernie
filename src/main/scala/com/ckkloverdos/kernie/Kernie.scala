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
import internal._


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

  private def _newInstanceOf(
      cls: Class[_],
      instances: mutable.LinkedHashSet[AnyRef],
      instanceByClass: mutable.LinkedHashMap[Class[_], AnyRef],
      nest: Int,
      debugContext: String,
      format: String,
      args: Any*
  ): AnyRef = {
    val nesting = "  " * nest
    logger.debug("%s%s Instantiating %s".format(nesting, debugContext, cls.getSimpleName))
    val instance = Catch(cls.newInstance().asInstanceOf[AnyRef])(format, args: _*)
    instances += instance
    instanceByClass += cls -> instance
    instance
  }

  private def _computeServiceInstance(
      cls: Class[_],
      instances: mutable.LinkedHashSet[AnyRef],
      instanceByClass: mutable.LinkedHashMap[Class[_], AnyRef],
      implByAPI: collection.Map[Class[_], Class[_]],
      nest: Int,
      debugContext: String,
      format: String,
      args: Any*
  ): AnyRef = {
    val nesting = "  " * nest
    logger.debug("%sComputing service instance for %s".format(nesting, cls.getSimpleName))
    instanceByClass.get(cls) match {
      case Some(instance) ⇒
        // Service has already been computed
        instance

      case None ⇒
        implByAPI.get(cls) match {
          case Some(impl) ⇒
            // We need an interface, so get the binding
            logger.debug("%s%s Computing %s for %s".format(nesting, debugContext, impl.getSimpleName, cls.getSimpleName))
            _newInstanceOf(impl, instances, instanceByClass, nest + 1, debugContext, format, args:_*)

          case None ⇒
            if(cls.isInterface) {
              throw new KernieException(format + " " + debugContext, args:_*)
            }
            else {
              _newInstanceOf(cls, instances, instanceByClass, nest + 1, debugContext, format, args:_*)
            }
        }
    }
  }

  private def _injectDependencies(
    initialServiceInfo: InitialServiceInfo,
    bindingInfo: BindingInfo,
    dependencyInfo: DependencyInfo
  ) {
    val instances = initialServiceInfo.instances
    val instanceByClass = initialServiceInfo.instanceByClass
    val serviceClasses = dependencyInfo.serviceClasses
    val immediateClassDependencies = dependencyInfo.immediateClassDependencies
    val linearizedDependencies = dependencyInfo.linearizedDependencies
    val implByAPI = bindingInfo.implByAPI

    for {
      cls ← linearizedDependencies
      clsDeps ← immediateClassDependencies.get(cls)
      field ← clsDeps.fieldsToInject
      fieldType = field.getType
    } {
      logger.debug("Injecting [%s:] %s into %s".format(field.getName, fieldType.getSimpleName, cls.getSimpleName))

      val clsInstance = _computeServiceInstance(
        cls,
        instances,
        instanceByClass,
        implByAPI,
        1,
        "[owner service %s]".format(cls.getSimpleName),
        "Could not create instance of owner service %s",
        cls.getSimpleName
      )

      val fieldValue = _computeServiceInstance(
        fieldType,
        instances,
        instanceByClass,
        implByAPI,
        1,
        "[owner service %s]".format(cls.getSimpleName),
        "Could not create instance of injected service %s",
        fieldType.getSimpleName
      )

      Catch {
        field.set(clsInstance, fieldValue)
      }("Could not set field [%s:] %s of %s", field.getName, fieldType.getSimpleName, cls.getSimpleName)
    }
  }

  private def _checkBindings(bindings: Seq[Binding[_]]) {
    val apiGroups = bindings.groupBy(_.api)
    val duplicateAPIs = apiGroups.filter(_._2.size > 1)
    for((api, classes) ← duplicateAPIs) {
      throw new KernieException(
        "%s is bound to multiple implementations: %s",
        api.getSimpleName,
        classes.map(_.impl.getSimpleName).mkString(", "))
    }
  }

  private def _computeBindingInfo(bindings: Seq[Binding[_]]): BindingInfo = {
    _checkBindings(bindings)

    val apiAndImpls = bindings.map(_.toUntypedTuple)
    val apis = bindings.map(_.untypedAPI)
    val impls = bindings.map(_.untypedImpl)

    val implByAPI = mutable.LinkedHashMap[Class[_], Class[_]](apiAndImpls:_*)
    val apiClasses = mutable.LinkedHashSet[Class[_]](apis:_*)
    val implClasses = mutable.LinkedHashSet[Class[_]](impls:_*)

    BindingInfo(implByAPI, apiClasses, implClasses)
  }

  private def _logClasses(format: String, serviceClasses: collection.Set[Class[_]]) {
    logger.debug(format.format(serviceClasses.map(_.getSimpleName).mkString(", ")))
  }

  private def _init(descriptions: Seq[AnyRef]) {
    if(logger.isDebugEnabled) {
      logger.debug("%s Descriptions: %s".format(descriptions.size, descriptions.mkString(", ")))
    }

    val (bindings0, services) = descriptions.partition(_.isInstanceOf[Binding[_]])
    val bindings = bindings0.map(_.asInstanceOf[Binding[_]])
    val bindingInfo = _computeBindingInfo(bindings)

    val initialServiceInfo = _initialServiceInfoOf(services)
    val initialServiceClasses = new mutable.LinkedHashSet[Class[_]] ++
      bindingInfo.implClasses ++
      initialServiceInfo.instanceByClass.keys
    val dependencyInfo = _computeDependencyInfo(initialServiceClasses)

    if(logger.isDebugEnabled()) {
      _logClasses("Initial: %s", initialServiceClasses)

      val newServiceClasses = dependencyInfo.serviceClasses -- initialServiceClasses
      _logClasses("New: %s", newServiceClasses)

      logger.debug("Linearized dependencies: %s".format(
        dependencyInfo.
          linearizedDependencies.
          map(_.getSimpleName).
          zipWithIndex.map{case (a, b) ⇒ (b, a)}.
          mkString(", "))
      )
    }

    _injectDependencies(initialServiceInfo, bindingInfo, dependencyInfo)
  }
}
