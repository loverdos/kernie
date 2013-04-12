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

import internal._
import java.lang.reflect.Field
import javax.inject.Inject
import scala.collection.mutable
import com.ckkloverdos.kernie.log.KernieLogger


/**
 * All services must be singletons by design.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
final class Kernie(
    logger: KernieLogger,
    classLoader: ClassLoader,
    descriptions: AnyRef*
) {
  if(classLoader eq null) {
    throw new KernieException("null ClassLoader")
  }

  private[this] var _injectionInfo: InjectionInfo = _

  _init(descriptions)

  private def LOG(fmt: String, args: Any*) {
    if(logger ne null) {
      try logger.log(fmt, args:_*)
      catch { case _: Throwable ⇒ }
    }
  }

  private def _isFieldToInject(field: Field) =
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
          LOG("%sMust inject '%s' %s into %s",
            "  " * nest,
            field.getName,
            field.getType.getSimpleName,
            cls.getSimpleName
          )

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
        path: ImmutableLinkedSet[Class[_]],
        deps: mutable.LinkedHashSet[Class[_]],
        nest: Int
    ) {
      val nesting = "  " * nest
      if(explored.contains(cls)) {
        LOG("%sAlready explored %s", nesting, cls.getSimpleName)
        return
      }

      LOG("%sExploring %s, path is %s",
        nesting,
        cls.getSimpleName,
        path.map(_.getSimpleName).mkString(" -> ")
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
      explore(cls, new ImmutableLinkedSet(cls), linearizedDeps, 0)
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
    LOG("%s%s Instantiating %s", "  " * nest, debugContext, cls.getSimpleName)

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
    LOG("%sComputing service instance for %s", nesting, cls.getSimpleName)

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
                  LOG("%s%s Computing %s for %s",
                    nesting,
                    debugContext,
                    implClass.getSimpleName,
                    cls.getSimpleName
                  )

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
          LOG("%s%s Computing %s",
            nesting,
            debugContext,
            cls.getSimpleName
          )

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
      LOG("Injecting '%s' %s into %s", field.getName, fieldType.getSimpleName, cls.getSimpleName)

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
      }("Could not set field '%s' %s of %s", field.getName, fieldType.getSimpleName, cls.getSimpleName)
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
    LOG(format, serviceClasses.map(_.getSimpleName).mkString(", "))
  }

  private def _getDescriptionInfo(descriptions: Seq[AnyRef]): DescriptionInfo = {
    var implClasses: collection.Seq[Class[_]] = mutable.Seq()
    var bindings: collection.Seq[Binding[_]] = mutable.Seq()
    var instances: collection.Seq[AnyRef] = mutable.Seq[AnyRef]()

    def addImplClass(cls: Class[_]) { implClasses +:= cls }
    def addBinding(binding: Binding[_]) {
      addImplClass(binding.impl.asInstanceOf[Class[_]])
      bindings +:= binding
      LOG("%s", binding)
    }
    def addInstance(instance: AnyRef) { instances +:= instance }

    for((description, index) ← descriptions.zipWithIndex) {

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

        case t @ Tuple2(apiName: String, impl: Class[_]) ⇒
          Catch {
            val api = loadAPIClass(classLoader, apiName)
            val binding = Binding.dynamicByClass(api, impl)
            addBinding(binding)
          } ("Bad binding in description %s", t)

        case t @ Tuple2(api: Class[_], implName: String) ⇒
          Catch {
            val impl = loadImplClass(classLoader, implName)
            val binding = Binding.dynamicByClass(api, impl)
            addBinding(binding)
          } ("Bad binding in description %s", t)

        case t @ Tuple2(apiName: String, implInstance: AnyRef) ⇒
          Catch {
            val api = loadAPIClass(classLoader, apiName)
            val impl = implInstance.getClass
            val binding = Binding.dynamicByClass(api, impl)
            addBinding(binding)
            addInstance(implInstance)
          } ("Bad binding in description %s", t)

        case t @ Tuple2(api: Class[_], impl: Class[_]) ⇒
          Catch {
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

  private def _runLifecycleActionOnInstance(
      instance: AnyRef,
      actionName: String,
      run: Lifecycle ⇒ Unit
  ) {
    instance match {
      case instance: Lifecycle ⇒
        val serviceClass = instance.getClass
        val serviceClassName = serviceClass.getSimpleName
        LOG("Running %s on instance of %s", actionName, serviceClassName)
        Catch(run(instance))("Error running %s on instance of %s", actionName, serviceClassName)

      case _ ⇒
    }
  }

  private def _getInjectedInstance(
      cls: Class[_],
      instanceByClass: mutable.LinkedHashMap[Class[_], AnyRef],
      implByAPI: mutable.LinkedHashMap[Class[_], Class[_]]
  ): Option[AnyRef] = {

    // Gets an injected instance for the given class.
    // Note that the class may be either an interface or an implementation,
    // so this is a two-step procedure
    instanceByClass.get(cls) match {
      case someInstance @ Some(_) ⇒
        someInstance

      case None ⇒
        implByAPI.get(cls) match {
          case Some(implClass) ⇒
            instanceByClass.get(implClass)

          case None ⇒
            None
        }
    }
  }

  private def _runLifecycleActionOnAll(injectionInfo: InjectionInfo, actionName: String, run: Lifecycle ⇒ Unit) {
    import injectionInfo.{linearizedDependencies, instanceByClass, implByAPI}
    // We need this because service classes are registered either as interfaces or implementation classes,
    // so the same instance will be found if we query either for the interface or the implementation
    // of a binding
    val alreadyRun = mutable.LinkedHashSet.empty[AnyRef]

    for {
      serviceClass ← linearizedDependencies
    } {

      _getInjectedInstance(serviceClass, instanceByClass, implByAPI) match {
        case None ⇒
          throw new KernieException(
            "Could not find an instance of %s to run %s",
            serviceClass.getSimpleName,
            actionName
          )

        case Some(instance) ⇒
          if(!alreadyRun(instance)) {
            _runLifecycleActionOnInstance(instance, actionName, run)
            alreadyRun += instance
          }
      }
    }

    LOG("Successfully ran %s on % instances", actionName, alreadyRun.size)

    alreadyRun.clear()
  }

  private def _configureServices(injectionInfo: InjectionInfo) {
    _runLifecycleActionOnAll(injectionInfo, "configure()", _.configure())
  }

  private def _startServices(injectionInfo: InjectionInfo) {
    _runLifecycleActionOnAll(injectionInfo, "start()", _.start())
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
    LOG("I have %s service classes for which to compute dependencies", initialServiceClasses.size)

    val dependencyInfo = _computeDependencyInfo(initialServiceClasses)

    _logClasses("Initial: %s", initialServiceClasses)

    val newServiceClasses = dependencyInfo.serviceClasses -- initialServiceClasses
    _logClasses("New: %s", newServiceClasses)

    LOG("%s Linearized dependencies: %s",
      dependencyInfo.linearizedDependencies.size,
      dependencyInfo.linearizedDependencies.
        map(_.getSimpleName).
        zipWithIndex.map{case (a, b) ⇒ (b, a)}.
        mkString(", ")
    )

    val injectionInfo = _injectDependencies(initialServiceInfo, bindingInfo, dependencyInfo)

    _configureServices(injectionInfo)
    _startServices(injectionInfo)

    this._injectionInfo = injectionInfo
  }

  // This is a generalization of serviceInstanceOfInterface
  def serviceInstanceOf[T](cls: Class[T]): T = {
    val instanceByClass = this._injectionInfo.instanceByClass
    val implByAPI = this._injectionInfo.implByAPI

    _getInjectedInstance(cls, instanceByClass, implByAPI) match {
      case Some(instance) ⇒
        instance.asInstanceOf[T]

      case None ⇒
        throw new KernieException("No service instance found for %s", cls)
    }
  }

  def serviceInstanceOfInterface[T](apiClass: Class[T]): T = {
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

  def bindings: List[Binding[_]] = {
    this._injectionInfo.implByAPI.map {
      case (api, impl) ⇒ Binding.dynamicByClass(api, impl)
    }.toList
  }
}
