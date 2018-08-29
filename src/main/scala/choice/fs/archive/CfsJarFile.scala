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
package choice.fs.archive

import net.liftweb.common._
import net.liftweb.util.Helpers._
import choice.fs._
import choice.lib.FileOps
import choice.model._
import java.io.ByteArrayOutputStream
import java.util.jar._
import java.util.zip.ZipOutputStream
import java.util.zip.ZipEntry

import choice.access.{Principal, RightDef, RightsCheck}
import choice.fs.vfs.VFile
import net.liftweb.mapper.BaseMetaMapper

/**
 * Trait for a builder of an archive file. The type of the archive output stream is S.
 * The resulting archive file is a subclass of OldDbFile indicated by A.
 */
trait DbArchiveBuilder[S, A] {

    /**
     * This is the main entry point for the builder. Its arguments include the input file
     * path, and the output file path. The folder that will contain the output archive file
     * may not exist. In that case, the 'create' parameter indicates whether it should be
     * created, and 'recursive' indicates if the path to the output folder should be
     * created recursively.
     * 
     * The 'incdir' parameter indicates that if the input path names a folder, whether
     * that folder name should have an entry in the archive. If incdir is true, the
     * first entry will be the input folder, and the paths to all other entries will
     * start with the input folder name.
     * 
     * The 'filter' parameter specifies an optional filter to select which files should
     * go into the archive. Files for which the filter returns false will be ignored,
     * including all descendant files in the case of a folder.
     * 
     */
    def build(inputPath : CfsPath, outputPath : CfsPath, create : Boolean, recursive : Boolean,
              incdir : Boolean, by : Principal, filter : VFile ⇒ Boolean = _ ⇒ true) : Box[A] = {
        val infile = getInputFile(inputPath, by)
        val outFolder = getOutputFolder(outputPath, create, recursive, by)
        infile.flatMap { fin ⇒
            val result = outFolder.flatMap { fout ⇒
                val bstream = new ByteArrayOutputStream
                val outbox = createOutputStream(bstream).flatMap { outstream ⇒
                    processInputFiles(outstream, fin, "", incdir, filter)
                    finish(outputPath, fout, outstream, bstream)
                }
                fout close ()
                outbox
            }
            fin close ()
            result
        }
    }

    def build(inputPath : CfsPath, incdir : Boolean, by : Principal, filter : VFile ⇒ Boolean) : Box[Array[Byte]] = {
        getInputFile(inputPath, by).flatMap { fin ⇒
            val bstream = new ByteArrayOutputStream
            val result = createOutputStream(bstream).flatMap { outstream ⇒
                processInputFiles(outstream, fin, "", incdir, filter)
                finish(outstream, bstream)
            }
            fin close ()
            result
        }
    }
    
    def getInputFile(inputPath : CfsPath, by : Principal) : Box[VFile] = Cfs open (inputPath, by, CfsOpenOptions.Default)

    def getOutputFolder(outputPath : CfsPath, create : Boolean, recursive : Boolean, by : Principal) : Box[CfsFolder] = {
        val fpath = outputPath.getParent
        Cfs open (fpath, by, CfsOpenOptions.Default) match {
            case Full(folder : CfsFolder) ⇒ Full(folder)
            case Full(dbfile : CfsFile) ⇒ Failure("invalid output folder " + dbfile.getPath.toString)
            case Full(_) ⇒ Failure("operation not supported for non-Cfs folder")
            case Empty ⇒
                if (create) FileOps.makeFolder(fpath, by, recursive)
                else Failure("output folder " + fpath.toString + " does not exist")
            case f : Failure ⇒ f
        }
    }

    /**
     * Create the archive output stream on a ByteArrayOutputStream.
     */
    def createOutputStream(bytesOut : ByteArrayOutputStream) : Box[S]

    /**
     * Create an entry for a specified file and name. The name is the relative
     * path to the file from the top level of the archive being created.
     */
    def createEntry(out : S, file : CfsFile, name : String) : Unit

    /**
     * Write the file contents for an entry.
     */
    def writeEntryContent(out : S, file : CfsPlain) : Unit

    def processInputFiles(out : S, file : VFile, prefix : String, incdir : Boolean, filter : VFile ⇒ Boolean) : Unit = {
        def helper(file : VFile, prefix : String, incdir : Boolean, closefile : Boolean) : Unit = {
            if (filter(file)) file match {
                case folder : CfsFolder ⇒
                    val name = prefix + folder.getName + "/"
                    val newprefix = if (incdir) {
                        createEntry(out, folder, name)
                        name
                    }
                    else prefix
                    val (folders, files) = folder.getFoldersAndFiles
                    folders.foreach(f ⇒ helper(f, newprefix, incdir = true, closefile = true))
                    files.foreach(f ⇒ helper(f, newprefix, incdir = true, closefile = true))
                case dbfile : CfsPlain ⇒
                    val name = prefix + dbfile.getName
                    createEntry(out, dbfile, name)
                    writeEntryContent(out, dbfile)
                case _ ⇒
            }
            if (closefile) {
                file close ()
            }
        }
        helper(file, "", incdir = incdir, closefile = false)
    }

    /**
     * Close the archive output stream, get the resulting bytes from the byte stream,
     * and create the archive file.
     */
    def finish(outputPath : CfsPath, outFolder : CfsFolder, out : S, bstream : ByteArrayOutputStream) : Box[A]
    
    def finish(out : S, bstream : ByteArrayOutputStream) : Box[Array[Byte]]
}

trait ZipArchiveBuilder extends DbArchiveBuilder[ZipOutputStream, CfsFile] {

    def createOutputStream(bytesOut : ByteArrayOutputStream) : Box[ZipOutputStream] = {
        tryo(new ZipOutputStream(bytesOut))
    }

    def createEntry(out : ZipOutputStream, file : CfsFile, name : String) : Unit = {
        file info () foreach { info ⇒
            val entry = new ZipEntry(name)
            entry.setTime(info.mtime)
            out.putNextEntry(entry)
        }
    }

    def writeEntryContent(out : ZipOutputStream, file : CfsPlain) : Unit = {
        file copyToStream out
    }

    def finish(outputPath : CfsPath, outFolder : CfsFolder,
               out : ZipOutputStream, bstream : ByteArrayOutputStream) : Box[CfsFile] = {
        out.close()
        val content = bstream.toByteArray
        val (dataSeqnum, _) = ChoiceData makeDataFile content
        val options = CfsCreateOptions(dataSeqnum = dataSeqnum, replace = false)
        outFolder create (outputPath.getFileName.toString, "application/zip", options)
    }

    def finish(out : ZipOutputStream, bstream : ByteArrayOutputStream) : Box[Array[Byte]] = {
        out.close()
        Full(bstream.toByteArray)
    }
}

object DbZip extends ZipArchiveBuilder {
    def apply(inputPath : CfsPath, outputPath : CfsPath, create : Boolean, recursive : Boolean,
              incdir : Boolean, by : Principal, filter : VFile ⇒ Boolean) : Box[CfsFile] = {
        build(inputPath, outputPath, create, recursive, incdir, by, filter)
    }

    def apply(inputPath : CfsPath, incdir : Boolean, by : Principal,
              filter : VFile ⇒ Boolean = _ ⇒ true) : Box[Array[Byte]] =
        build(inputPath, incdir, by, filter)
}

case class CfsJarNode(resource : Resource) extends CfsSpecialNode(resource) {
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
    override def cfsOpen(path : CfsAbsolutePath, principal : Principal,
                         options : CfsOpenOptions) : Box[CfsJarFile] = {
        Full(new CfsJarFile (path, principal, this))
    }
}

class CfsJarFile(path : CfsAbsolutePath, principal : Principal, vnode : CfsJarNode)
                extends CfsSpecial(path, principal, vnode) {


}

object CfsJarFile extends MimeTypeHandler with DbArchiveBuilder[JarOutputStream, CfsJarFile] {

    override protected[archive] val Log : Logger = Logger("choice.fs.CfsJarFile")

    override val getName : String = "Jar File Type"

    override val getSchemas : List[BaseMetaMapper]  = Nil

    override val getMimeType = "application/x-java-archive"

    override val isContainer_? = false

    /**
     * Return the list of access rights for this MIME type.
     *
     * @return a list of all the access rights for files of this MIME type
     */
    override def getRights : List[RightDef] = CfsPlain.getRights

    override def instantiate(resource : Resource) : Box[CfsJarNode] = {
        if (isType_?(resource)) Full(CfsJarNode(resource))
        else Failure(s"resource id ${resource.getSafeKey} is not a jar node")
    }

    def invalidate(resid : ResourceId, mtid : MimeTypeId) : Unit = {}
    
    /**
     * Make a .jar file from a file or files at a given input path.
     *
     * @param inpath path to input file or files
     * @param outpath path to output jar file to be created
     * @param fentry if true and the inpath names a folder, an entry for the folder is included.
     *               Otherwise the default is to make entries for the files in the folder, but
     *               not the folder itself.
     * @param filter a filter that is passed each candidate input file. Files for which the
     *               filter returns false are not included in the jar file.
     * @return a boxed CfsJarFile for the created jar file
     */
    def apply(inpath : CfsPath, outpath : CfsPath, by : Principal, fentry : Boolean = false,
              filter : VFile ⇒ Boolean = _ ⇒ true) : Box[CfsJarFile] = {
        Log.info("CfsJarFile: inpath=" + inpath.toString + ", outpath=" + outpath.toString +
                 ", fentry=" + fentry)
        build(inpath, outpath, create = false, recursive = false, incdir = fentry, by = by, filter = filter)
    }

    def createOutputStream(bytesOut : ByteArrayOutputStream) : Box[JarOutputStream] = {
        val manifest = new Manifest
        manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
        tryo(new JarOutputStream(bytesOut, manifest))
    }

    def createEntry(out : JarOutputStream, file : CfsFile, name : String) : Unit = {
        file info () foreach { info ⇒
            val entry = new JarEntry(name)
            entry.setTime(info.mtime)
            out.putNextEntry(entry)
        }
    }

    def writeEntryContent(out : JarOutputStream, file : CfsPlain) : Unit = {
        file copyToStream out
    }

    def finish(outputPath : CfsPath, outFolder : CfsFolder,
               out : JarOutputStream, bstream : ByteArrayOutputStream) : Box[CfsJarFile] = {
        out.close()
        val content = bstream.toByteArray
        val (dataSeqnum, _) = ChoiceData makeDataFile content
        val options = CfsCreateOptions(replace = false, dataSeqnum = dataSeqnum)
        outFolder create (outputPath.getFileName.toString, getMimeType, options) match {
            case Full(dbjar : CfsJarFile) ⇒ Full(dbjar)
            case Full(_) ⇒ Failure("FileManager createFile did not return a jar file")
            case e : EmptyBox ⇒ e
        }
    }
    
    def finish(out : JarOutputStream, bstream : ByteArrayOutputStream) : Box[Array[Byte]] = {
        out.close()
        Full(bstream.toByteArray)
    }

    /**
     * This defines the rights needed to create a new instance of this object type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canCreateObject : RightsCheck = CfsPlain.canCreateObject

    /**
     * This defines the rights needed to unlink an object of this type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canUnlinkObject : RightsCheck = CfsPlain.canUnlinkObject
}
