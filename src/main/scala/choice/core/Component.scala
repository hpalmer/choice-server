/**
  * Copyright © 2013-2017 The Board of Trustees of The Leland Stanford Junior University.
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *     http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */
/**
  * A Component is a collection of content that comprises an application or a
  * part of an application. The processing of resolving a URL to a file starts
  * with a Component, which contains Routes that match some prefix of the URL.
  * The target of a Route may be a file, a folder, or another Component.
  *
  * Routes of a component are ordered. URL resolution looks for the first Route
  * that produces a file that can be served. A depth-first search is used.
  *
  * @author Howard Palmer
  */
package choice.core

import choice.access.{RightDef, _}
import choice.actor.SessionManager
import choice.fs._
import choice.model.{GlobalConfig, Resource}
import net.liftweb.common.{Full, _}
import net.liftweb.http._
import net.liftweb.mapper.BaseMetaMapper
import net.liftweb.util.Helpers.tryo


final case class ComponentDescriptor(routes : List[Route], group : Option[String])

class ComponentNode(resource : Resource) extends CfsSpecialNode(resource)
                                         with AtomicDataNode[ComponentNode]
                                         with JsonDataNode[ComponentNode, ComponentDescriptor] {
    val mf : Manifest[ComponentDescriptor] = manifest[ComponentDescriptor]

    /**
     * Return true if this file is a container, i.e. supports the lookup() operation.
     * All Cfs files are containers, even if only for metadata files.
     *
     * @return true if this Vnode represents a container
     */
    override def isContainer_? : Boolean = false

    /**
     * Create a file handle (VFile or its subclass) for a resource of this MIME type.
     *
     * @param path the filename path used to locate the file
     * @param principal the principal responsible for this open operation
     * @param options options that may affect the open operation
     * @return a boxed VFile (or subclass) for the file associated with the vnode. A Failure
     *         may be returned for various errors, such as insufficient access rights, or
     *         unsupported options.
     */
    override def cfsOpen(path : CfsAbsolutePath, principal : Principal, options : CfsOpenOptions) : Box[Component] = {
        Full(new Component(path, principal, this))
    }
}

class Component(path : CfsAbsolutePath, principal : Principal, wnode : ComponentNode)
    extends CfsSpecial(path, principal, wnode)
    with JsonDataFile[Component, ComponentNode, ComponentDescriptor] {

    import choice.core.Component.Log

    def getData : Box[ComponentDescriptor] = (Component canReadComponent this) (() ⇒ getDataUnchecked)

    def putData(data : ComponentDescriptor) : Box[Boolean] = (Component canWriteComponent this) (() ⇒ putDataUnchecked (data))

    /**
     * Resolve the remaining portion of a URL, relative to this component.
     *
     * @param urlParts the remaining URL parts
     * @param urlIndex the number of parts of the original URL that have matched so far
     * @param origUrl the original URL
     * @param principal the principal responsible for this operation
     * @return a RouteResult
     */
    def resolve(urlParts : List[String], urlIndex : Int, origUrl : List[String], principal : Principal) : RouteResult = {
        val endSlash = S.request map (_.path.endSlash) openOr false
        if (urlIndex > 0) {
            val contextPath = origUrl.take(urlIndex).mkString("/", "/", "")
            RequestContextPath.set(Full(contextPath))
        }
        getData match {
            case Full(desc) ⇒
                // Helper to step through routes of this component
                def nextRoute(routes : List[Route]) : RouteResult = {
                    routes match {
                        case Nil ⇒ NoRoute
                        case currentRoute :: tail ⇒
                            // Check whether the next route matches a prefix of the remaining URL
                            currentRoute checkMatch urlParts match {
                                case None ⇒ nextRoute(tail)
                                case Some(partsLeft) ⇒
                                    val nextUrlIndex = urlIndex + currentRoute.length
                                    val targetPath = currentRoute absolutePath path

                                    // Helper to resolve the file referenced by the route path. If the file exists,
                                    // it could be a plain file, a folder, or another component. If it's a folder
                                    // or a component, try to resolve the remaining URL parts relative to it.
                                    def resolveFile(fpath : CfsPath, nUrlIndex : Int, partsLeft : List[String]) : RouteResult = {
                                        Cfs open(fpath, principal, CfsOpenOptions.Default) match {
                                            case Full(comp : Component) ⇒
                                                val result =
                                                    if (partsLeft == Nil) {
                                                        if (endSlash) {
                                                            comp resolve (List("index"), nUrlIndex, origUrl, principal)
                                                        }
                                                        else RouteRedirect(S.uri + "/")
                                                    }
                                                    else {
                                                        comp resolve(partsLeft, nUrlIndex, origUrl, principal)
                                                    }
                                                // Close the component unless it was used in a RoutePlainFile
                                                result match {
                                                    case RoutePlainFile(_, c) if c.getResourceId == comp.getResourceId ⇒
                                                    case _ ⇒ comp close ()
                                                }
                                                result
                                            case Full(folder : CfsFolder) ⇒
                                                Log.debug(s"resolveFile: found folder ${folder.getPath.toString}")
                                                folder close()
                                                // Take the next remaining part of the URL as the name of a file
                                                // in this folder. If no remaining part, try "index".
                                                partsLeft match {
                                                    case Nil ⇒
                                                        if (endSlash) resolveFile(fpath / "index", nUrlIndex, Nil)
                                                        else RouteRedirect(S.uri + "/")
                                                    case phead :: ptail ⇒
                                                        tryo (fpath / phead) match {
                                                            case Full(nextpath) ⇒ resolveFile(nextpath, nUrlIndex + 1, ptail)
                                                            case _ : EmptyBox ⇒
                                                                Log.error(s"Bad path component in URL: '$phead'")
                                                                NoRoute
                                                        }
                                                }
                                            case Full(file : CfsPlain) ⇒
                                                // Cfs open doesn't check access rights, so do that here
                                                (CfsPlain canGetInputStream file) { () ⇒
                                                    Full(RoutePlainFile(file, Component.this))
                                                } match {
                                                    case Full(rr) ⇒ rr
                                                    case AuthNeededFailure ⇒
                                                        file close ()
                                                        RouteNeedsLogin(Empty, urlIndex, fpath)
                                                    case _ ⇒
                                                        file close ()
                                                        RouteFailure(Failure(s"cannot read $fpath"))
                                                }
                                            case Full(vfile) ⇒
                                                vfile close()
                                                RouteFailure(Failure(s"${fpath.toString} is not a Cfs file"))
                                            case Empty ⇒
                                                // If this is the last component of the URL, and it doesn't have
                                                // a suffix, try to find an HTML/XML file. Otherwise no route.
                                                val baseName = fpath.getFileName.toString
                                                if (partsLeft != Nil || baseName.contains(".")) {
                                                    Log.debug(s"resolveFile: no file ${fpath.toString}")
                                                    nextRoute(tail)
                                                }
                                                else {
                                                    def suffixHelper(suffixes : List[String]) : RouteResult = {
                                                        suffixes match {
                                                            case Nil ⇒
                                                                NoRoute
                                                            case shead :: stail ⇒
                                                                val spath = fpath.getParent / (baseName + shead)
                                                                resolveFile(spath, nUrlIndex, Nil) match {
                                                                    case NoRoute ⇒ suffixHelper(stail)
                                                                    case rr ⇒ rr
                                                                }
                                                        }
                                                    }
                                                    suffixHelper(List(".html", ".htm", ".xml"))
                                                }
                                            case f : Failure if f == AuthNeededFailure ⇒
                                                // Build a RouteNeedsLogin instance. This may be modified as the
                                                // resolution stack unwinds, to contain information from the last
                                                // encountered component which had an associated group. This will
                                                // be the default group for login, and the login page will be
                                                // found through that component.
                                                RouteNeedsLogin(Empty, urlIndex, fpath)
                                            case f : Failure ⇒ RouteFailure(f)
                                        }
                                    }
                                    resolveFile(targetPath, nextUrlIndex, partsLeft)
                            }
                    }
                }
                nextRoute(desc.routes) match {
                    case rr @ RouteNeedsLogin(Empty, _, fpath) ⇒
                        desc.group match {
                            case Some(group) ⇒ RouteNeedsLogin(GroupInfo(group, SystemPrincipal), urlIndex, fpath)
                            case None ⇒ rr
                        }
                    case rr ⇒ rr
                }
            case e : EmptyBox ⇒
                Log.error(s"component descriptor missing for ${getPath.toString}", e)
                NoRoute

        }
    }
}

object Component extends MimeTypeHandler {
    val Log = Logger("choice.core.Component")

    val defaultComponentFile = "default.cco"
    val defaultComponentDescriptor = ComponentDescriptor(List(Route(Nil, RoutePath(Nil, absolute = true))), None)
//    val defaultComponentDescriptor = ComponentDescriptor(
//        // "fschoice" as the servlet context is normally removed by Lift before
//        // the point where this component would be used. However, there may be
//        // links which include "fschoice" still in use even when the servlet
//        // context is ROOT. So including a route for "fschoice" should not be
//        // a problem, as long as someone doesn't try to create a folder, /fschoice.
//        List(Route(List("fschoice"), RoutePath(Nil, absolute = true)),
//            Route(Nil, RoutePath(Nil, absolute = true))), None)

    override def getName = "Component File Type"

    override val getSchemas : List[BaseMetaMapper] = Nil

    /**
     * Return the list of access rights for this MIME type.
     *
     * @return a list of all the access rights for files of this MIME type
     */
    override val getRights : List[RightDef] = List(
        RightDef("create_component", "component file", "create a component"),
        RightDef("unlink_component", "component file", "unlink a component"),
        RightDef("read_component", "component file", "read component definition"),
        RightDef("write_component", "component file", "write component definition")
    )

    //    def apply(path : VPath, dbresource : ComponentResource, principal : Principal) : Box[Component] =
    //        instantiate(path, dbresource, principal)

    override def getMimeType : String = "choice/component"

    override def isContainer_? : Boolean = false

    /**
     * Construct a folder Vnode from a DB Resource object.
     *
     * @param resource the Resource object as stored in the DB
     * @return if the resource is compatible with this MIME type, a boxed wrapper for the
     *         given resource, otherwise Failure
     */
    override def instantiate(resource : Resource) : Box[ComponentNode] = {
        if (isType_?(resource)) Full(new ComponentNode (resource))
        else Failure(s"failed to instantiate resource id ${resource.getSafeKey} as a ComponentNode")
    }


    def resolveRequest : PartialFunction[Req, () ⇒ Box[LiftResponse]] = {
        case req @ (Req(_, _, GetRequest) | Req(_, _, HeadRequest)) ⇒
            val principal = SessionManager.getSessionClient(create = false) map (_.getPrincipal) getOrElse GuestPrincipal
            val getResponse = getDefaultComponent match {
                case Full(comp) ⇒
                    val response = comp resolve (req.path.wholePath, 0, req.path.wholePath, principal) match {
                        case route : RoutePlainFile ⇒
                            if (!(comp eq route.component)) comp close ()
                            route.getLiftResponse(principal)
                        case route : RouteNeedsLogin ⇒
                            comp close ()
                            route.getLiftResponse(principal)
                        case route : RouteRedirect ⇒
                            comp close ()
                            route.getLiftResponse(principal)
                        case NoRoute ⇒
                            comp close ()
                            NoRoute.getLiftResponse(principal)
                        case fail : RouteFailure ⇒
                            comp close ()
                            fail.getLiftResponse(principal)
                    }
                    Full(() ⇒ response)
                case Empty ⇒ Failure("default component is missing")
                case f : Failure ⇒ f
            }
            getResponse match {
                case Full(respftn) ⇒ respftn
                case e : EmptyBox ⇒
                    () ⇒ Full(RouteFailureResponse(e))
            }
    }

    def getDefaultComponent : Box[Component] = {
        val sfolder = GlobalConfig.getSystemFolder()
        val result = sfolder getMember defaultComponentFile match {
            case Full(component : Component) ⇒ Full(component)
//            case Full(component : Component) ⇒
//                component.getDataUnchecked match {
//                    case Full(desc) ⇒
//                        // Check whether the "fschoice" route is in the existing component
//                        if (desc.routes.exists(_.urlParts == List("fschoice"))) {
//                            Full(component)
//                        }
//                        else {
//                            // No, recreate the component
//                            sfolder unlinkUnchecked (component, recursive = false)
//                            component close ()
//                            getDefaultComponent
//                        }
//                    case _ : EmptyBox ⇒
//                        sfolder unlinkUnchecked (component, recursive = false)
//                        component close ()
//                        getDefaultComponent
//                }
            case Full(_) ⇒ Failure(s"'$defaultComponentFile' is not a component")
            case Empty ⇒
                val path = sfolder.getPath / defaultComponentFile
                sfolder create(defaultComponentFile, Component.getMimeType, CfsCreateOptions.Default) match {
                    case Full(component : Component) ⇒
                        component putData defaultComponentDescriptor
                        Log.info(s"default component created in '$path'")
                        Full(component)
                    case Full(_) ⇒ Failure(s"default component creation hosed")
                    case e : EmptyBox ⇒
                        val fail = Failure(s"failed to create default component in ${path.toString}", Empty, e)
                        Log.error(fail)
                        fail
                }
            case f : Failure ⇒ f
        }
        sfolder close ()
        result
    }

    /**
     * This method is called by DbManager when it is about to delete a file with a MIME
     * type associated with this MimeTypeHandler. It gives the MimeTypeHandler an
     * opportunity to veto the delete, or to do other cleanup operations, such as
     * removing non-filesystem references to the file.
     *
     * Note that this is called on the DbManager thread, so it should only use the
     * dbPlugin interface to access DbManager functions.
     *
     * @param vnode the vnode of the file about to be deleted
     * @return a boxed value of true if the delete should proceed, otherwise a
     *         boxed false or EmptyBox
     */
    override def delete(vnode: CfsVnode): Box[Boolean] = {
        // Deleting a component can change URL mappings to files in complex ways.
        // So nothing in the cache can be trusted.
        CacheFilter clear ()
        super.delete(vnode)
    }

    private val _canCreateObject : RightsCheck = AnyRightsCheck("create_component")

    /**
     * This defines the rights needed to create a new instance of this object type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    override def canCreateObject : RightsCheck = {
        // Creating a component can change URL mappings to files in complex ways.
        // There is no guarantee that a component actually will get created, since
        // access control has yet to be checked here, but this was the easiest
        // place to clear the cache.
        CacheFilter clear ()
        _canCreateObject
    }

    /**
     * This defines the rights needed to unlink an object of this type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canUnlinkObject : RightsCheck = AnyRightsCheck("unlink_component")

    val canReadComponent = AnyRightsCheck("read_component")
    val canWriteComponent = AnyRightsCheck("write_component")
}
