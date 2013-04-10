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
class Kernie(classLoader: ClassLoader, descriptions: AnyRef*) {
  if(classLoader eq null) {
    throw new KernieException("null ClassLoader")
  }

  private[this] val logger = LoggerFactory.getLogger(this.getClass)
  private[this] var _injectionInfo: InjectionInfo = _

  _init(descriptions)

  def _isFieldToInject(field: Field) =
    field.getAnnotation(classOf[Inject]) ne null

  private def _initialServiceInfoOf(initial: Seq[AnyRef]): InitialServiceInfo = {
    val foundServices = new mutable.LinkedHashSet[AnyRef]
    val foundServiceByClass = new mutable.LinkedHashMap[Class[_], AnyRef]

    for((service, index) ← initial.zipWithIndex) {
      if(service eq null) {
        val extraInfo = extraIndexInfo[AnyRef](initial, index, _.getClass.getSimpleName)

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
        if(logger.isDebugEnabled) {
          logger.debug("%sAlready explored %s".format("  " * nest, cls.getSimpleName))
        }
        return
      }

      if(logger.isDebugEnabled) {
        logger.debug("%sExploring %s, path is %s".format(
          "  " * nest,
          cls.getSimpleName,
          path.map(_.getSimpleName).mkString(" -> "))
        )
      }

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
    if(logger.isDebugEnabled) {
      val nesting = "  " * nest
      logger.debug("%s%s Instantiating %s".format(nesting, debugContext, cls.getSimpleName))
    }

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
    if(logger.isDebugEnabled) {
      val nesting = "  " * nest
      logger.debug("%sComputing service instance for %s".format(nesting, cls.getSimpleName))
    }

    // See if we already know any service instance for the given class
    instanceByClass.get(cls) match {
      case Some(instance) ⇒
        // Yes, service has already been computed
        instance

      case None ⇒
        // No, we must compute it now
        if(cls.isInterface) {
          // It's an interface (API), let's grab the implementation class
          implByAPI.get(cls) match {
            case Some(implClass) ⇒
              // Do we have an instance for the implementation class?
              instanceByClass.get(implClass) match {
                case Some(implInstance) ⇒
                  implInstance

                case None ⇒
                  // Nope, so we must create an instance
                  if(logger.isDebugEnabled) {
                    val nesting = "  " * nest
                    logger.debug("%s%s Computing %s for %s".format(
                      nesting,
                      debugContext,
                      implClass.getSimpleName,
                      cls.getSimpleName))
                  }

                  _newInstanceOf(implClass, instances, instanceByClass, nest + 1, debugContext, format, args:_*)
              }

            case None ⇒
              // Signal an error, since we have a missing binding
              throw new KernieException(
                format + " " + debugContext + ". No binding for %s", (args ++ Seq(cls.getSimpleName)):_*)
          }
        }
        else {
          // It's an implementation class. Just compute the service instance
          if(logger.isDebugEnabled) {
            val nesting = "  " * nest
            logger.debug("%s%s Computing %s".format(
              nesting,
              debugContext,
              cls.getSimpleName))
          }

          _newInstanceOf(cls, instances, instanceByClass, nest + 1, debugContext, format, args:_*)
        }
    }
  }

  private def _injectDependencies(
    initialServiceInfo: InitialServiceInfo,
    bindingInfo: BindingInfo,
    dependencyInfo: DependencyInfo
  ): InjectionInfo = {

    val instances = initialServiceInfo.instances
    val instanceByClass = initialServiceInfo.instanceByClass
    val immediateClassDependencies = dependencyInfo.immediateClassDependencies
    val linearizedDependencies = dependencyInfo.linearizedDependencies
    val implByAPI = bindingInfo.implByAPI

    for {
      cls ← linearizedDependencies
      clsDeps ← immediateClassDependencies.get(cls)
      field ← clsDeps.fieldsToInject
      fieldType = field.getType
    } {
      if(logger.isDebugEnabled) {
        logger.debug("Injecting [%s:] %s into %s".format(field.getName, fieldType.getSimpleName, cls.getSimpleName))
      }

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

    InjectionInfo(
      instances = instances,
      instanceByClass = instanceByClass,
      immediateClassDependencies = immediateClassDependencies,
      linearizedDependencies = linearizedDependencies,
      implByAPI = implByAPI
    )
  }

  private def _checkMultipleImplementations(bindings: Seq[Binding[_]]) {
    val apiGroups = bindings.groupBy(_.api)
    val duplicateAPIs = apiGroups.filter(_._2.size > 1)
    for((api, classes) ← duplicateAPIs) {
      throw new KernieException(
        "%s is bound to multiple implementations: %s",
        api.getSimpleName,
        classes.map(_.impl.getSimpleName).mkString(", "))
    }
  }

  private def _computeAndPostCheckBindingInfo(bindings: Seq[Binding[_]]): BindingInfo = {
    _checkMultipleImplementations(bindings)

    val apiAndImpls = bindings.map(_.toUntypedTuple)
    val implByAPI = mutable.LinkedHashMap[Class[_], Class[_]](apiAndImpls:_*)

    BindingInfo(implByAPI)
  }

  private def _logClasses(format: String, serviceClasses: collection.Set[Class[_]]) {
    logger.debug(format.format(serviceClasses.map(_.getSimpleName).mkString(", ")))
  }

  private def _getDescriptionInfo(descriptions: Seq[AnyRef]): DescriptionInfo = {
    var implClasses: collection.Seq[Class[_]] = mutable.Seq()
    var bindings: collection.Seq[Binding[_]] = mutable.Seq()
    var instances: collection.Seq[AnyRef] = mutable.Seq[AnyRef]()

    @inline def addImplClass(cls: Class[_]) { implClasses +:= cls }
    @inline def addBinding(binding: Binding[_]) { bindings +:= binding }
    @inline def addInstance(instance: AnyRef) { instances +:= instance }

    for((description, index) ← descriptions.zipWithIndex) {
      if(logger.isDebugEnabled) {
        logger.debug(
          "Description %s of type %s".format(
            description,
            if(description eq null) null else description.getClass)
        )
      }

      def atIndexExtraInfo: String =
        "at index %s%s".format(index, extraIndexInfo[AnyRef](descriptions, index, _.toString))

      def kidding(what: AnyRef) =
        throw new KernieException("Are you kidding me with %s %s?", what, atIndexExtraInfo)

      description match {
        case null ⇒
          throw new KernieException("null description %s", atIndexExtraInfo)

        case instance: java.lang.Number ⇒
          kidding(instance)

        case cls: Class[_] if cls.isPrimitive ⇒
          kidding(cls)

        case ex: Throwable ⇒
          kidding(ex)

        case Tuple2(null, null) ⇒
          throw new KernieException("Null binding %s", atIndexExtraInfo)

        case Tuple2(api, null) ⇒
          throw new KernieException("Null implementation binding for %s %s", api, atIndexExtraInfo)

        case Tuple2(null, impl) ⇒
          throw new KernieException("Null api for implementation %s %s", impl, atIndexExtraInfo)

        case className: CharSequence ⇒
          val cls = loadImplClass(classLoader, className.toString)
          addImplClass(cls)

        case cls: Class[_] ⇒
          addImplClass(cls)

        case binding: Binding[_] ⇒
          addBinding(binding)

        case t @ Tuple2(apiName: String, implName: String) ⇒
          Catch {
            val binding = Binding.dynamicByName(classLoader, apiName, implName)
            addBinding(binding)
          } ("Bad binding in description %s", t)

        case t @ Tuple2(api: Class[_], impl: Class[_]) ⇒
          Catch {
            val binding = Binding.dynamicByClass(api, impl)
            addBinding(binding)
          } ("Bad binding in description %s", t)

        case t @ Tuple2(api: Class[_], implName: String) ⇒
          Catch {
            val impl = loadImplClass(classLoader, implName)
            val binding = Binding.dynamicByClass(api, impl)
            addBinding(binding)
          } ("Bad binding in description %s", t)

        case t @ Tuple2(api: Class[_], implInstance: AnyRef) ⇒
          Catch {
            val impl = implInstance.getClass
            val binding = Binding.dynamicByClass(api, impl)
            addBinding(binding)
            addInstance(implInstance)
          } ("Bad binding in description %s",  t)

        case instance: AnyRef if instance.getClass.getName.startsWith("scala.Tuple") ⇒
          kidding(instance)

        case instance: AnyRef ⇒
          addInstance(instance)
      }
    }

    DescriptionInfo(
      implClasses = implClasses,
      bindings = bindings,
      instances = instances
    )
  }

  private def _init(descriptions: Seq[AnyRef]) {
    val descriptionInfo = _getDescriptionInfo(descriptions)

    val bindings = descriptionInfo.bindings
    val instances = descriptionInfo.instances
    val serviceClasses = descriptionInfo.implClasses

    val bindingInfo = _computeAndPostCheckBindingInfo(bindings)

    val initialServiceInfo = _initialServiceInfoOf(instances)
    val initialServiceClasses = new mutable.LinkedHashSet[Class[_]] ++
      initialServiceInfo.instanceByClass.keys ++
      serviceClasses
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

    this._injectionInfo = _injectDependencies(initialServiceInfo, bindingInfo, dependencyInfo)
//    _configureServices()
  }

  def serviceByAPI[T](apiClass: Class[T]): T = {
    if(apiClass eq null) {
      throw new KernieException("null interface")
    }
    else if(!apiClass.isInterface) {
      throw new KernieException("%s is not an interface", apiClass)
    }

    this._injectionInfo.implByAPI.get(apiClass) match {
      case Some(implClass) ⇒
        this._injectionInfo.instanceByClass.get(implClass) match {
          case Some(instance) ⇒
            instance.asInstanceOf[T]

          case None ⇒
            throw new KernieException("No instance found for %s implementing %s", implClass, apiClass)
        }

      case None ⇒
        throw new KernieException("No implementation class found for %s", apiClass)
    }
  }
}
