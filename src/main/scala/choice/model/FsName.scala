/**
  * Copyright © 2012-2017 The Board of Trustees of The Leland Stanford Junior University.
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
  * @author Howard Palmer
  */
package choice.model

import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._


/**
 * This maps a name component of a file path to a resource.
 */
class FsName extends LongKeyedMapper[FsName] with IdPK with HasSafeKey[FsNameId] {

    def getSingleton : FsName.type = FsName

    /**
     * This references the folder resource which contains the named resource.
     */
    object folder extends MappedLongForeignKey(this, Resource)

    /**
     * This references a Resource which has the name in this entry in some folder.
     * The same Resource may be referenced by multiple FsName entries, if the
     * Resource is contained in multiple folders.
     */
    object resource extends MappedLongForeignKey(this, Resource)

    /**
     * This is the name of the Resource, relative to some folder.
     */
    object name extends MappedString(this, 255) {
        // Make the name case-sensitive
        override def fieldCreatorString(dbType: DriverType, colName : String): String = {
            s"$colName ${dbType.varcharColumnType(maxLen)} collate utf8_bin ${notNullAppender()}"
        }

        override def dbNotNull_? = true
        override def dbIndexed_? = true
    }

    /** Get the key for this entry wrapped as a FsNameId */
    def getSafeKey : FsNameId = FsNameId(this.id.get)
    
    def getFolderId : ResourceId = ResourceId(this.folder.get)
    
    def getResourceId : ResourceId = ResourceId(this.resource.get)
    
//    /**
//     * Get a list of the FsName entries for files in this folder.
//     *
//     * @return a list of FsName entries
//     */
//    def listFiles : List[FsName] = FsName.findAll(By(FsName.folder, this.resource.is))

//    /**
//     * LookupRequest a name in the folder represented by the current FsName. The name is
//     * assumed to be a single name component of a file path.
//     *
//     * @param name	a name to lookup in the current directory
//     *
//     * @return a boxed FsName if the name is found, otherwise Empty or a Failure
//     */
//    def lookup(name : String) : Box[FsName] = {
//        if (name == null || name.length == 0) Failure("name missing in lookup")
//        else FsName.find(By(FsName.folder, resource.is), By(FsName.name, name))
//    }

//    def lookup(path : List[String]) : Box[FsName] = {
//        @tailrec
//        def helper(list : List[String], fsn : FsName) : Box[FsName] = {
//            list match {
//                case Nil ⇒ Full(fsn)
//                case head :: tail ⇒
//                    fsn lookup head match {
//                        case Full(nfsn) ⇒ helper(tail, nfsn)
//                        case e : EmptyBox ⇒ e
//                    }
//            }
//        }
//        helper(path, this)
//    }
    
//    def contains(res : Resource) : Boolean = {
//        FsName.find(By(FsName.folder, resource.is), By(FsName.resource, res)).isDefined
//    }
    
//    def addName(name : String, res : Resource) : Box[FsName] = {
//        val result = tryo(FsName.create.folder(this.resource.is).name(name).resource(res).saveMe())
//        result match {
//            case Full(fsn) ⇒ this.resource.obj match {
//                case Full(myres) ⇒
//                    // Increment the folder reference count for each file added
//                    myres.addReference flatMap { _ ⇒ result }
//                case e : EmptyBox ⇒ e
//            }
//            case e : EmptyBox ⇒ e
//        }
//    }
    
//    def getContainer : Box[FsName] = {
//        folder.obj match {
//            case Full(fres) ⇒ FsName.find(By(FsName.resource, fres)) match {
//                case ffsn @ Full(_) ⇒ ffsn
//                case e : EmptyBox ⇒
//                    Failure("missing container for " + this.name.is, Empty, e)
//            }
//            case e : EmptyBox ⇒
//                Failure("missing container resource for " + this.name.get, Empty, e)
//        }
//    }
    
//    def getPath : List[String] = {
//        def ancestors(fsname : FsName) : Stream[FsName] = {
//            fsname.getContainer match {
//                case Full(parent) if parent.id.is == fsname.id.is ⇒ Stream.empty
//                case Full(parent) ⇒ fsname #:: ancestors(fsname)
//                case e : EmptyBox ⇒ Stream.empty
//            }
//        }
//        ancestors(this).foldLeft(List[String]()) { (list, fsn) ⇒ fsn.name.get :: list }
//    }
    
//    /**
//     * Create a link to the Resource referenced by this FsName entry. The reference
//     * count of the specified Resource is incremented, and a new FsName entry is
//     * created with the given linkname.
//     *
//     * @param linkfolder	the folder to which the link is to be added
//     * @param linkname		the name to be given to the resource in the new link
//     * @return boxed FsName created for the link
//     */
//    def link(linkfolder : FsName, linkname : String) : Box[FsName] = {
//        resource.obj match {
//            case Full(res) ⇒ {
//                res.addReference() match {
//                    case Full(count) ⇒ linkfolder.addName(linkname, res) match {
//                        case full @ Full(fsn) ⇒ full
//                        case e : EmptyBox ⇒
//                            res.removeReference()
//                            e
//                    }
//                    case e : EmptyBox ⇒ e ?~! "failed to increment Resource reference count"
//                }
//            }
//            case e : EmptyBox ⇒ e ?~! "link: Resource does not exist"
//        }
//    }

    /**
     * Remove a FsName entry for a Resource. The reference count for the Resource is
     * decremented. The Resource entry is not automatically removed when its reference
     * count goes to zero, since there could still be open file handles for the
     * associated file.
     *
     * @return the boxed Resource or a Failure
     */
    def unlink(resource : Resource) : Box[Resource] = {
        assert(resource.getSafeKey == this.getResourceId)
        val result = (resource removeReference () flatMap { count ⇒
            if (count < 0) Failure(s"resource ${resource.getSafeKey} reference count is negative: $count")
            else Full(resource)
        }) ?~ "unlink: Resource does not exist"
        tryo(delete_!)
        result
    }

}

object FsName extends FsName with LongKeyedMetaMapper[FsName] {
    
    protected val Log = Logger("choice.model.FsName")
    
    /**
     * Add an FsName entry which creates a containment relation between a folder
     * and member, with the member having a specified name. The folder and member
     * are both specified as Resource objects. The reference count of the member
     * Resource is assumed to have already been incremented for the FsName entry
     * being added. The caller is responsible for decrementing the member's
     * reference count if this operation fails.
     *
     * @param folder the folder Resource
     * @param name the name to be given the new member
     * @param member the member Resource
     * @return a boxed FsName if successful, otherwise Failure
     */
    def addName(folder : Resource, name : String, member : Resource) : Box[FsName] = {
        assert(member.getRefCount > 0)
        tryo(FsName.create.folder(folder).resource(member).name(name).saveMe())
    }

    /**
     * Get a list of all the FsName entries that reference a given resource as the
     * folder. These are entries for the files in the folder.
     */
    def containerLinks(resid : ResourceId) : List[FsName] = {
        FsName.findAll(By(FsName.folder, resid))
    }
    
    /**
     * Find all the FsName entries that reference a given resource. These are
     * all the names by which the resource is known in different containers.
     */
    def references(resid : ResourceId) : List[FsName] = {
        FsName.findAll(By(FsName.resource, resid))
    }
    
    /**
     * Find any reference to a given resource. There may be multiple references,
     * in which case the one returned is arbitrary.
     */
    def findResource(resid : ResourceId) : Box[FsName] = {
        FsName.find(By(FsName.resource, resid))
    }
    
    /**
     * Find an FsName for a specified resource as a member of a given container.
     * If it exists, it provides the name of the resource in that container.
     */
    def findResourceInContainer(resid : ResourceId, cresid : ResourceId) : Box[FsName] = {
        FsName.find(By(FsName.folder, cresid), By(FsName.resource, resid))
    }
    
    /**
     * Find an FsName entry for a given name in a specified container.
     */
    def findNameInContainer(name : String, resid : ResourceId) : Box[FsName] = {
        FsName.find(By(FsName.name, name), By(FsName.folder, resid))
    }
}