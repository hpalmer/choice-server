/**
  * Copyright © 2013-2016 The Board of Trustees of The Leland Stanford Junior University.
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
  * The choice/library file type.
  *
  * @author Howard Palmer
  */
package choice.fs

import choice.access.{AnyRightsCheck, Principal, RightDef, RightsCheck}
import choice.fs.archive.CfsJarFile
import choice.fs.vfs.VFile
import choice.lib.FileOps
import choice.model.Resource
import net.liftweb.common.{Full, _}
import net.liftweb.mapper.BaseMetaMapper
import net.liftweb.util.Helpers.tryo

import scala.xml._

/**
 * A Library is a special kind of Folder that is used for publishing versioned
 * components.
 */
class Library(path : CfsAbsolutePath, principal : Principal, libvnode : LibraryNode)
              extends CfsFolder(path, principal, libvnode)
              with AtomicDataFile[Library, LibraryNode] {
    import choice.fs.Library.Log

    override def getVnode : LibraryNode = libvnode

    def getBytes : Box[Array[Byte]] = (Library canReadDescription this) (() ⇒ getBytesUnchecked)

    def putBytes(data : Array[Byte]) : Box[Boolean] = (Library canWriteDescription this) { () ⇒
        putBytesUnchecked (data) map (_ ⇒ true)
    }

    def publish(groupId : String, artifactId : String, version : String, path : CfsPath) : Box[NodeSeq] = {
        def makeName = s"$groupId.$artifactId.v$version"
        Log.info("publishing " + makeName)
        val deps = Library.getDependencies(path, principal) match {
            case Full(deplist) if deplist != Nil ⇒
                <dependencies>
                    { for (dep ← deplist) yield dep.toXML }
                </dependencies>
            case _ ⇒ NodeSeq.Empty
        }
        val repoRoot = getPath
        CfsPath(groupId.split('.').toList.reverse ::: List(artifactId, version), repoRoot) match {
            case Full(repoPath) ⇒
                Log.info(s"publishing in ${repoPath.toString}")
                Cfs open (repoPath, principal, CfsOpenOptions.Default) match {
                    case Full(vfile) ⇒
                        vfile close ()
                        Failure(makeName + " has already been published")
                    case Empty ⇒
                        FileOps.makeFolder(repoPath, principal, recursive = true).flatMap { rfolder ⇒
                            val result : Box[NodeSeq] = CfsJarFile(path, repoPath / "content.jar", principal, fentry = false) match {
                                case Full(dbjar : CfsJarFile) ⇒
                                    val xml = <component>
                                        <groupId>{ groupId }</groupId>
                                        <artifactId>{ artifactId }</artifactId>
                                        <version>{ version }</version>
                                        <library>{ getPath.toString }</library>
                                        { deps }
                                    </component>
                                    // The rfolder Resource reference count gets incremented by the creation
                                    // of the jar file above. So the copy in the rfolder Folder is now stale.
                                    // So reload it. Obviously things should be changed so that there is only
                                    // one copy of a Resource (or any other DB object) in memory.
                                    val pp = new PrettyPrinter(80, 2)
                                    val xmlbytes = pp.formatNodes(xml).getBytes
                                    val (dataSeqnum, _) = ChoiceData.makeDataFile(xmlbytes)
                                    val options = CfsCreateOptions(dataSeqnum = dataSeqnum, replace = false)
                                    val nodeseq : Box[NodeSeq] =
                                        rfolder create ("component.xml", "text/xml", options) match {
                                            case Full(dbfile) ⇒
                                                dbfile close ()
                                                Full(xml)
                                            case e : EmptyBox ⇒ e
                                        }
                                    dbjar close ()
                                    nodeseq
                                case e : EmptyBox ⇒ Failure("failed to create content.jar", Empty, e)
                            }
                            val finalResult = result match {
                                case Full(_) ⇒ result
                                case e : EmptyBox ⇒
                                    rfolder.withParentFile() { _ unlink (rfolder.getName, recursive = true) }
                                    e
                            }
                            rfolder close ()
                            finalResult
                        }
                    case f : Failure ⇒ f
                }
            case e : EmptyBox ⇒ e
        }
    }
}

case class Dependency(groupId : String, artifactId : String, version : Option[String],
                      library : Option[String], localRef : Option[String]) {

    def makeOpt(elem : Elem, value : Option[String]) : NodeSeq = {
        value match {
            case Some(v) ⇒ elem.copy(child = Text(v))
            case None ⇒ NodeSeq.Empty
        }
    }

    def toXML : Elem = {
        <dependency>
            <groupId>{ groupId }</groupId>
            <artifactId>{ artifactId }</artifactId>
            { makeOpt(<version/>, version) }
            { makeOpt(<library/>, library) }
            { makeOpt(<localRef/>, localRef) }
        </dependency>
    }

}

object Dependency {
    val Log = Logger("choice.fs.Dependency")

    def apply(file : CfsPlain) : Box[List[Dependency]] = {
        def stringToOption(s : String) : Option[String] = if (s == "") None else Some(s)

        Log.info("reading dependency file " + file.getPath.toString)
        tryo(CfsFiles.newInputStream(file)) flatMap { xmlin ⇒
            tryo(XML.load(xmlin)).flatMap { xml ⇒
                Log.info("decoding XML: " + xml.toString)
                val deplist = xml filter (node ⇒ node.label == "dependency") flatMap { dep ⇒
                    Log.info("found a dependency")
                    val groupId = (dep \ "groupId").text
                    val artifactId = (dep \ "artifactId").text
                    val version = stringToOption((dep \ "version").text)
                    val library = stringToOption((dep \ "library").text)
                    val localRef = stringToOption((dep \ "localRef").text)
                    Full(new Dependency(groupId, artifactId, version, library, localRef))
                }
                Full(deplist.toList)
            }
        } match {
            case full @ Full(xml) ⇒
                Log.info("read " + xml.toString)
                full
            case Empty ⇒
                Log.info("result is Empty")
                Empty
            case f : Failure ⇒
                Log.error(f)
                f
        }
    }
}

case class LibraryNode(resource : Resource) extends CfsFnode(resource) with AtomicDataNode[LibraryNode] {
    /**
     * Create a file handle (VFile or its subclass) for a resource of this MIME type.
     *
     * @param path the filename path used to locate the file
     * @param principal the principal responsible for this open operation
     * @param options options that may affect the open operation
     * @return a boxed Library. A Failure may be returned for various errors, such as
     *         insufficient access rights, or unsupported options.
     */
    override def cfsOpen(path : CfsAbsolutePath, principal : Principal,
                         options : CfsOpenOptions) : Box[Library] = {
        Full(new Library(path, principal, this))
    }
}

object Library extends CfsFolderHandler[LibraryNode, Library] {

    override protected val Log = Logger("choice.fs.Library")

    override val getMimeType = "choice/library"

    override val name = "library"
    override def makeNode(resource : Resource) : Box[LibraryNode] = Full(LibraryNode(resource))
//    override def makeFile(path : CfsPath, principal : Principal, vnode : LibraryNode) : Box[Library] = {
//        Full(new Library(path, principal, vnode))
//    }

    override val getName = "Library File Type"

    override val getSchemas : List[BaseMetaMapper] = Nil


    override val rights : List[RightDef] = List(
        RightDef("create_library", "library container files", "create a library"),
        RightDef("unlink_library", "library container files", "unlink a library"),
        RightDef("read_library_desc", "library container files", "read library description"),
        RightDef("list_library", "library container files", "list members"),
        RightDef("write_library_desc", "library container files", "write library description"),
        RightDef("add_to_library", "library container files", "create a new member"),
        RightDef("remove_from_library", "library container files", "remove a member")
    )

    def create(path : CfsPath, principal : Principal) : Box[Library] = {
        val ppath = path.getParent
        Cfs.withExistingFile(ppath, principal) {
            case folder : CfsFolder ⇒
                folder create (path.getFileName.toString, getMimeType, CfsCreateOptions.Default) match {
                    case Full(lib : Library) ⇒ Full(lib)
                    case Full(_) ⇒ Failure("FileManager createFile did not return a library")
                    case e : EmptyBox ⇒ e
                }
        }
    }

    def getDependencies(path : CfsPath, principal : Principal) : Box[List[Dependency]] = {
        Cfs open (path, principal, CfsOpenOptions.Default) match {
            case Full(folder : CfsFolder) ⇒
                def helper(vfiles : List[VFile], rlist : List[CfsPlain]) : List[CfsPlain] = {
                    vfiles match {
                        case Nil ⇒ rlist
                        case head :: tail ⇒ head match {
                            case cfsplain : CfsPlain if cfsplain.getName.endsWith(".dep") ⇒
                                helper(tail, cfsplain :: rlist)
                            case other ⇒
                                other close ()
                                helper(tail, rlist)
                        }
                    }
                }
                val deplist = helper(folder.getPlainFiles, Nil) flatMap { depfile ⇒
                    Dependency(depfile) openOr Nil
                }
                Full(deplist)
            case _ ⇒ Empty
        }
    }

    /**
     * This defines the rights needed to list members of the container, or to reference
     * them by name.
     *
     * @return a RightsCheck instance
     */
    val canListMember : RightsCheck = AnyRightsCheck("list_library")

    /**
     * This defines the rights needed to add a member to the container, either by creating
     * a new file, or linking to an existing file.
     *
     * @return a RightsCheck instance
     */
    val canAddMember : RightsCheck = AnyRightsCheck("add_to_library")

    /**
     * This defines the rights needed to unlink a member from the container.
     *
     * @return a RightsCheck instance
     */
    val canRemoveMember : RightsCheck = AnyRightsCheck("remove_from_library")

    /**
     * This defines the rights needed to create a new instance of this object type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canCreateObject : RightsCheck = AnyRightsCheck("create_library")

    /**
     * This defines the rights needed to unlink an object of this type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canUnlinkObject : RightsCheck = AnyRightsCheck("unlink_library")

    val canReadDescription = AnyRightsCheck(rights map (_.name) : _*)
    val canWriteDescription = AnyRightsCheck("write_library_desc", "add_to_library", "remove_from_library")
}