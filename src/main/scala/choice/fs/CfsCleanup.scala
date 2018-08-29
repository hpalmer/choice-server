/**
  * Copyright © 2015-2016 The Board of Trustees of The Leland Stanford Junior University.
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
  * This object contains various methods used for Cfs filesystem maintenance.
  *
  * @author Howard Palmer
  * Created by Hep on 7/12/2015.
  */
package choice.fs

import java.nio.file.Files

import choice.access.Principal
import choice.actor.DbManager
import choice.model._
import net.liftweb.common._
import net.liftweb.mapper.{By, DB}
import net.liftweb.util.DefaultConnectionIdentifier
import net.liftweb.util.Helpers._

import scala.collection.mutable.ListBuffer

object CfsCleanup {
    private val Log = Logger("choice.fs.CfsCleanup")

    def getChoiceDataIterator[T](f : Iterator[Long] ⇒ T) : T = {
        import scala.collection.JavaConverters.asScalaIteratorConverter
        // Retrieve all of the existing ChoiceData sequence numbers
        val dirstream = Files.newDirectoryStream(ChoiceData.getDataDir)
        try {
            val iter = dirstream.iterator.asScala
            f(for {
                path ← iter
                fname = path.getFileName.toString
                fparts = fname split '.'
                seqnum ← tryo(fparts(0).toLong)
            } yield seqnum)
        }
        finally {
            dirstream close ()
        }
    }

    /**
     * Look for ChoiceData files which are not being referenced from the filesystem, or which
     * are being referenced more than once. Normally each ChoiceData file is referenced by a
     * single DataNode. However, in certain cases where a ChoiceData file is open for output,
     * it may be referenced from a Resource directly, before a DataNode has been created for
     * it. This looks for ChoiceData files which either are not being referenced from anywhere,
     * or are referenced from multiple places.
     *
     * @param takeAction true if such files should be removed
     * @param principal principal for this operation (not currently used)
     * @return a tuple containing an array of orphan ChoiceData sequence numbers, and an
     *         array of 2-element arrays for each sequence number referenced by more than
     *         one DataNode, with each 2-element array containing the sequence number
     *         followed by the count of references (which will be > 1).
     */
    def choiceDataOrphans(takeAction : Boolean, principal : Principal)
                                : Box[(List[Long], List[List[Long]])] = {
        if (principal.isSystemAdmin_?) {
            if (takeAction) {
                DbManager clearFsNameCache ()
                DbManager clearVNodeCache ()
            }
            // Get all the DataNode seqnum fields and make a map of seqnum to number of references
            val dnseqnums = DataNode.findAll().map(_.seqnum.get).groupBy(identity).mapValues(_.size)
            // Find any seqnums referenced by more than one DataNode. This should never happen.
            val multirefs = dnseqnums filter {
                case (_, count) ⇒ count > 1
            }
            // Find ChoiceData seqnums not referenced by any DataNode or by any Resource
            val orphans = getChoiceDataIterator { cdseqnums ⇒
                val norefs = cdseqnums filterNot { seqnum ⇒
                    (dnseqnums contains seqnum) || {
                        // This could be optimized more using SQL "WHERE seqnum IN list" to
                        // find all the norefs referenced by a Resource, and then filtering
                        // them out.
                        val resbox = Resource find By(Resource.seqnum, Full(seqnum))
                        resbox.isDefined
                    }
                }
                norefs.toList
            }
            val dupes = (multirefs map { case (seqnum, count) ⇒ List(seqnum, count.toLong) }).toList
            if (takeAction) {
                // Remove orphan ChoiceData files
                orphans foreach { seqnum ⇒ ChoiceData removeDataFile seqnum }
            }
            Full((orphans, dupes))
        }
        else Failure("operation restricted to system administrators")
    }

    /**
     * Check the refcount field of DataNodes to see if it has the correct count of Resource
     * objects referencing the DataNode.
     *
     * @param takeAction true if DataNode refcounts should be corrected
     * @param principal the principal for this operation
     * @return a boxed list of typles containing a DataNode id, it's current refcount, and
     *         the correct refcount, where the current and correct refcounts differ
     */
    def datanodeRefcounts(takeAction : Boolean, principal : Principal) : Box[List[(DataNodeId, Long, Long)]] = {
        if (principal.isSystemAdmin_?) {
            if (takeAction) {
                DbManager clearFsNameCache ()
                DbManager clearVNodeCache ()
            }
            // Somewhat hairy SQL to get actual number of Resources referencing each DataNode,
            // finding cases where that doesn't match the count in the DataNode.
            val pstmt =
                """
                  |select * from
                  | (select d.id as id, d.refcount as refcount, count(r.id) as arefcount from datanode d
                  |  left join resource_t r on r.dnode = d.id group by d.id) j where j.refcount != j.arefcount;
                """.stripMargin
            val tuples = DB.use(DefaultConnectionIdentifier) { conn ⇒
                DB.prepareStatement(pstmt, conn) { ps ⇒
                    DB.exec(ps) { rs ⇒
                        val lb = new ListBuffer[(DataNodeId, Long, Long)]()
                        while (rs.next()) {
                            val dnode = DataNodeId(rs.getLong(1))
                            val current = rs.getLong(2)
                            val actual = rs.getLong(3)
                            lb.append((dnode, current, actual))
                        }
                        lb.toList
                    }
                }
            }
            if (takeAction) {
                tuples foreach {
                    case (dnode, current, actual) ⇒
                        DataNode.findByKey(dnode.id) foreach { dn ⇒
                            if (dn.refcount.get == current) tryo {
                                // Deleting the DataNode is likely to leave a ChoiceData orphan,
                                // but that's what choiceDataOrphans() is for.
                                if (actual == 0L) DataNode.deleteDataNode(dn, deleteData = false)
                                else dn.refcount(actual).saveMe()
                            }
                        }
                }
            }
            Full(tuples)
        }
        else Failure("operation restricted to system administrators")
    }

    /**
     * Find Resource objects that are not referenced by at least one FsName resource field.
     *
     * @param takeAction true if any such Resources should be removed
     * @param principal the principal for this operation
     * @return a boxed list of Resource ids that are not referenced
     */
    def resourceOrphans(takeAction : Boolean, principal : Principal) : Box[List[ResourceId]] = {
        if (principal.isSystemAdmin_?) {
            if (takeAction) {
                DbManager clearFsNameCache ()
                DbManager clearVNodeCache ()
            }
            val pstmt =
                """select r.id from fsname f right join resource_t r on f.resource_c = r.id
              | where f.id is null
            """.stripMargin
            val resids = DB.use(DefaultConnectionIdentifier) { conn ⇒
                DB.prepareStatement(pstmt, conn) { ps ⇒
                    DB.exec(ps) { rs ⇒
                        val lb = new ListBuffer[ResourceId]()
                        while (rs.next()) {
                            lb += rs.getLong(1)
                        }
                        lb.toList
                    }
                }
            }
            if (takeAction) {
                resids foreach { rid ⇒
                    DbManager lookupById CfsVFileId(rid) match {
                        case Full(vnode) ⇒
                            val resource = vnode.getResource
                            tryo(resource.refcount(0L).saveMe())
                            vnode.release
                        case _ : EmptyBox ⇒
                            Log.error(s"unable to open vnode for resource ${rid.id}")
                    }
                }
            }
            Full(resids)
        }
        else Failure("operation restricted to system administrators")
    }

    /**
     * Check that the refcount fields in Resource objects reflects the number of FsName entries
     * that reference the Resource via their resource field.
     *
     * @param takeAction true if any incorrect Resource refcount fields should be corrected
     * @param principal the principal for this operation
     * @return a boxed list of typles containing a Resource id, it's current refcount, and
     *         the correct refcount, where the current and correct refcounts differ
     */
    def resourceRefcounts(takeAction : Boolean, principal : Principal) : Box[List[(ResourceId, Long, Long)]] = {
        if (principal.isSystemAdmin_?) {
            if (takeAction) {
                DbManager clearFsNameCache ()
                DbManager clearVNodeCache ()
            }
            // More hairy SQL to determine how many FsNames reference each Resource, and
            // return cases where this doesn't match the Resource refcount.
            val pstmt =
                """
                  |select * from
                  | (select r.id as id, r.refcount as refcount, count(r.id) as arefcount from
                  |   resource_t r left join fsname f on f.resource_c = r.id group by r.id) j
                  |    where j.refcount != j.arefcount;
                """.stripMargin
            val tuples = DB.use(DefaultConnectionIdentifier) { conn ⇒
                DB.prepareStatement(pstmt, conn) { ps ⇒
                    DB.exec(ps) { rs ⇒
                        val lb = new ListBuffer[(ResourceId, Long, Long)]()
                        while (rs.next()) {
                            val resource = ResourceId(rs.getLong(1))
                            val current = rs.getLong(2)
                            val actual = rs.getLong(3)
                            lb.append((resource, current, actual))
                        }
                        lb.toList
                    }
                }
            }
            if (takeAction) {
                tuples foreach {
                    case (resource, current, actual) ⇒
                        DbManager lookupById CfsVFileId(resource) match {
                            case Full(vnode) ⇒
                                val res = vnode.getResource
                                if (res.getRefCount == current) {
                                    tryo(res.refcount(actual).saveMe())
                                }
                                vnode.release
                            case _ : EmptyBox ⇒
                        }
                }
            }
            Full(tuples)
        }
        else Failure("operation restricted to system administrators")
    }

    def choiceDataMissing(takeAction : Boolean, principal : Principal)
                : Box[(List[(DataNodeId, Long)], List[(ResourceId, Long)])] = {
        if (principal.isSystemAdmin_?) {
            if (takeAction) {
                DbManager clearFsNameCache ()
                DbManager clearVNodeCache ()
            }
            val cdseqnums = getChoiceDataIterator(_.toSet)
            val dnmissing = DataNode findAll () map (dn ⇒ (dn.getSafeKey, dn.seqnum.get)) filterNot {
                case (_, seqnum) ⇒ cdseqnums contains seqnum
            }
            val remissing = Resource findAll () map (r ⇒ (r.getSafeKey, r.getSeqnum)) filterNot {
                case (_, seqnum) ⇒ seqnum <= 0L || (cdseqnums contains seqnum)
            }
            Full((dnmissing, remissing))
        }
        else Failure("operation restricted to system administrators")
    }

//    def fsnameResources(takeAction :Boolean, principal :Principal) : Box[List[CfsAbsolutePath]] = {
//        val pstmt =
//        """select f.id,f.folder,f.resource_c,f.name from fsname f left join resource_t r on f.resource_c=r.id
//          | where r.id is null
//        """.stripMargin
//        val tuples = DB.use(DefaultConnectionIdentifier) { conn ⇒
//            DB.prepareStatement(pstmt, conn) { ps ⇒
//                DB.exec(ps) { rs ⇒
//                    val lb = new ListBuffer[(FsNameId, ResourceId, ResourceId, String)]()
//                    while (rs.next()) {
//                        val fsname = FsNameId(rs.getLong(1))
//                        val folder = ResourceId(rs.getLong(2))
//                        val resource = ResourceId(rs.getLong(3))
//                        val name = rs.getString(4)
//                        lb.append((fsname, folder, resource, name))
//                    }
//                    lb.toList
//                }
//            }
//        }
//
//    }

}
