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
  * @author Howard Palmer
  */
package choice.fs

import java.io._
import java.nio.file._
import java.security.{DigestInputStream, MessageDigest}

import choice.core.Startup
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props

///**
// * This holds information about a file that is being uploaded. The file data is
// * read from the input stream passed to the constructor, and stored in a host
// * file with a unique sequence number. Other information passed to the constructor
// * should be extracted from the upload headers.
// */
//class ChoiceFileParamHolder(override val name : String,
//        					override val mimeType : String,
//                            override val fileName : String,
//                            inputStream : InputStream)
//    extends FileParamHolder(name, mimeType, fileName) {
//
//    /** Assign a unique sequence number */
//    val dataSeqnum : Long = ChoiceData.getFileSeqNumber
//
//    /** The host file path to the data file */
//    val filePath : Path = ChoiceData.getPath(dataSeqnum)
//
//    /** Copy the uploaded data to the host file, and save the length */
//    val copylen : Long = {
//        try {
//            val len = Files.copy(inputStream, filePath)
//            inputStream close()
//            len
//        }
//        catch {
//            case ex : Throwable ⇒
//                println(ex.toString)
//                throw ex
//        }
//    }
//
//    /**
//     * Returns the contents of the uploaded file as a Byte array.
//     */
//    def file : Array[Byte] = Helpers.readWholeStream(fileStream)    // readWholeStream closes the stream
//
//    /**
//     * Returns an input stream that can be used to read the
//     * contents of the host file containing the uploaded file data.
//     */
//    def fileStream : InputStream = Files.newInputStream(filePath, StandardOpenOption.READ)
//
//    /**
//     * Returns the length of the uploaded file.
//     */
//    def length : Long = Files.size(filePath)
//
//    /**
//     * Delete the host file created for the upload. This is used if the upload is rejected.
//     */
//    def delete : Box[Unit] = tryo { Files.delete(filePath) }
//}

/**
 * This object manages a directory of host files, each identified by a unique sequence number.
 * It provides a thread-safe mechanism to store and retrieve data from such files.
 */
object ChoiceData extends Logger {

    /**
      * Name of the property which specifies the host folder to contain the Choice data files.
      */
    val HostDataFolder = "choice.HostDataFolder"

    val seqFilename = "seq.dat"
        
    private var dataDir : Path = _

    private var seqnoChecked : Boolean = false

    /**
     * If it doesn't already exist, create the host directory where sequence-numbered files
     * will be stored, Register a function with Lift to handle uploaded files by storing
     * the uploaded data in a sequence-numbered file.
     */
    def init : Boolean = {
        
		setDataDir()
		if (dataDir != null) {

//	        LiftRules.handleMimeFile = fileHolderMaker
	        true
		}
		else false
    }
    
    /** Given a sequence number, get a file path for the associated host file */
    def getPath(seqnum : Long) : Path = {
        dataDir.resolve(seqnum.toString + ".dat")
    }

    def getDataDir: Path = dataDir

    /** Return the data contained in a sequence-numbered file as an array of bytes */
    def getBytes(seqnum : Long) : Array[Byte] = {
        if (seqnum <= 0) Array[Byte]()
        else tryo {
            Files.readAllBytes(getPath(seqnum))
        } openOr Array[Byte]()
    }
    
    /** Return an input stream for reading a specified sequence-numbered file */
    def getInputStream(seqnum : Long) : Box[InputStream] = {
        if (seqnum <= 0) Full(new ByteArrayInputStream(Array[Byte]()))
        else tryo {
            new FileInputStream(getPath(seqnum).toFile)
        }
    }

    /**
     * Return a RandomAccessFile for accessing a specified sequence-numbered file.
     *
     * Care must be taken to do appropriate locking and updating of Cfs metadata
     * if the returned handle is used for writing.
     *
     * @param seqnum the ChoiceData file sequence number
     * @param mode mode string, one of:
     *      "r" 	Open for reading only. Invoking any of the write methods of the resulting
     *              object will cause an IOException to be thrown.
     *      "rw" 	Open for reading and writing. If the file does not already exist then
     *              an attempt will be made to create it.
     *      "rws" 	Open for reading and writing, as with "rw", and also require that every
     *              update to the file's content or metadata be written synchronously to the
     *              underlying storage device.
     *      "rwd"   Open for reading and writing, as with "rw", and also require that every
     *              update to the file's content be written synchronously to the underlying
     *              storage device.
     * @return a boxed RandomAccessFile
     */
    def getRandomAccessFile(seqnum : Long, mode : String) : Box[RandomAccessFile] = {
        if (seqnum <= 0) Failure(s"getRandomAccessFile($seqnum)")
        else tryo {
            new RandomAccessFile(getPath(seqnum).toFile, mode)
        }
    }

    /**
     * Return an output stream for writing a specified sequence-numbered file. By default,
     * the output stream is positioned to the beginning of the file, set to replace existing
     * data. The append option can be specified to position to the end of file to append new
     * data.
     *
     * @param seqnum the file sequence number
     * @param append true if the returned stream should be positioned to end of file
     * @return a boxed FileOutputStream, or Failure
     */
    def getOutputStream(seqnum : Long, append : Boolean = false) : Box[FileOutputStream] = {
        if (seqnum <= 0) {
            Failure(s"getOutputStream: invalid file sequence number $seqnum")
        }
        else tryo {
            new FileOutputStream(getPath(seqnum).toFile, append)
        }
    }

    /**
     * Read a sequence-numbered file, computing an MD5 hash of its contents.
     * Return the hash as an array of bytes, and the length of the file. This
     * is used for data deduplication.
     */
    def getHashAndSize(seqnum : Long) : (Array[Byte], Long) = {
        ChoiceData.getInputStream(seqnum) match {
            case Full(instream) ⇒
                val md5 = MessageDigest.getInstance("MD5")
                val digester = new DigestInputStream(instream, md5)
                val buf = new Array[Byte](8192)
                var size = 0L
                var done = false
                while (!done) {
                    val len = digester.read(buf, 0, buf.length)
                    if (len > 0) size += len
                    else done = true
                }
                val hash = digester.getMessageDigest.digest
                instream.close()
                (hash, size)
            case _ : EmptyBox ⇒ (Array[Byte](), 0L)
        }
    }
    
    /**
     * Make a sequence-numbered file from an array of bytes. A tuple is returned,
     * containing the sequence number assigned to the file, and the length of
     * the byte array. A sequence number less than zero indicates an error. A
     * zero sequence number indicates the specified content array had zero length.
     *
     * @param content byte array containing content for file
     * @return (dataSeqnum, length) the data file sequence number, length
     */
    def makeDataFile(content : Array[Byte]) : (Long, Long) = {
        val seqnum = getFileSeqNumber
        val path = getPath(seqnum)
        tryo {
            if (Files.exists(path)) {
                error(path.toString + " already exists")
                makeDataFile(content)
            }
            else {
                Files.write(path, content)
                (seqnum, content.length.toLong)
            }
        } match {
            case Full(pair) ⇒ pair
            case e : EmptyBox ⇒
                error(e)
                (-1, -1)
        }
    }

    /**
     * Make a sequence-numbered file from data read from a specified input stream.
     * The assigned sequence number and the length of the data are returned as a
     * tuple. A sequence number less than zero indicates an error. A zero sequence
     * number indicates the input stream was empty.
     */
    def makeDataFile(inputStream : InputStream) : (Long, Long) = {
        val seqnum = getFileSeqNumber
        val path = getPath(seqnum)
        tryo {
            if (Files.exists(path)) {
                error(path.toString + " already exists")
                makeDataFile(inputStream)
            }
            else {
                val length = Files.copy(inputStream, path)
                (seqnum, length)
            }
        } match {
            case Full(pair) ⇒ pair
            case e : EmptyBox ⇒
                error(e)
                (-1L, -1L)
        }
    }
    
    /**
     * Remove a specified sequence-numbered file. A zero sequence number is harmless.
     */
    def removeDataFile(seqnum : Long) : Boolean = {
        if (seqnum > 0) tryo { Files deleteIfExists getPath(seqnum) } openOr false
        else seqnum == 0
    }
    
//    /**
//     * This function is registered with Lift to handle files being uploaded.
//     */
//    def fileHolderMaker(fieldName : String, contentType : String,
//            			fileName : String, inputStream : InputStream) : ChoiceFileParamHolder = {
//        new ChoiceFileParamHolder(fieldName, contentType, fileName, inputStream)
//    }

    /**
     * Check to see if two sequence-numbered files contain the same data. The return
     * value is zero if they do, non-zero if not.
     */
    def compareFiles(seq1 : Long, seq2 : Long) : Int = {
        if (seq1 == seq2) 0
        else {
            val stream1 = getInputStream(seq1)
            val stream2 = getInputStream(seq2)
            (stream1, stream2) match {
                case (Full(s1), Full(s2)) ⇒
                    tryo {
                        val buf1 = new BufferedInputStream(s1, 8192)
                        val buf2 = new BufferedInputStream(s2, 8192)
                        var done = false
                        var result = 0
                        while (!done) {
                            val v1 = buf1.read
                            val v2 = buf2.read
                            if ((v1 != v2) || (v1 < 0) || (v2 < 0)) {
                                done = true
                                result = v1 - v2
                            }
                        }
                        buf1.close()
                        buf2.close()
                        result
                    } openOr -1
                case _ ⇒ -1
            }
        }
    }
    
    /**
     * If it doesn't already exist, create the file that tracks the next sequence number.
     * If there are sequence-numbered files already in the designated directory, the
     * next sequence number will be one larger than the highest-numbered file.
     */
    def createSeqFile() : Long = synchronized {
        if (!seqnoChecked) {
            // First see if there's dburl.txt file in the ChoiceData folder, and create it
            // if not. This is used to associate a ChoiceData folder with a particular
            // database, so that the server won't start if they don't match.
            val dburlPath = dataDir.resolve("dburl.txt")
            if (Files.exists(dburlPath)) {
                val rdr = Files.newBufferedReader(dburlPath)
                val dburl = rdr.readLine().trim()
                rdr close ()
                if (dburl != Startup.dbUrl) {
                    throw new RuntimeException(s"${dburlPath.toString} contains $dburl, but the database URL is ${Startup.dbUrl}")
                }
                else info(s"database URL is $dburl")
            }
            else {
                val wrt = Files.newBufferedWriter(dburlPath)
                val line = s"${Startup.dbUrl.trim}"
                wrt.write(s"$line\n")
                wrt close ()
                warn(s"created ${dburlPath.toString} containing $line")
            }
            // Now scan all the files in the specified ChoiceData directory.
            var maxseq = 0L
            // This can throw an exception if there is a problem with dataDir.
            // Hopefully that will abort server initialization.
            val dirstream = Files.newDirectoryStream(dataDir)
            val iter = dirstream.iterator
            try {
                while (iter.hasNext) {
                    val path = iter.next
                    val fname = path.getFileName.toString
                    val fparts = fname.split('.')
                    tryo {
                        val n = fparts(0).toLong
                        if (n > maxseq) maxseq = n
                    }
                }
            }
            finally {
                dirstream close ()
            }

            info("createSeqFile: maxseq = " + maxseq)

            // Now write the sequence number out to the file
            val seqfile = new RandomAccessFile(dataDir.resolve(seqFilename).toString, "rw")
            maxseq = maxseq + 1
            try {
                try {
                    seqfile.writeLong(maxseq)
                }
                finally {
                    seqfile.getChannel.force(true)
                }
            }
            finally {
                seqfile.close()
            }
            seqnoChecked = true
            maxseq
        }
        else 0L
    }

    /**
     * Assign the next sequence number. The sequence number file is locked, the value of the
     * next sequence number is read, and the next value is stored in the file.
     */
    def getFileSeqNumber : Long = {
        // Create the sequence number file if necessary
        if (!seqnoChecked) {
            createSeqFile()
        }
        // Increment the value in the sequence number file and return it.
        synchronized {
            val seqfile = new RandomAccessFile(dataDir.resolve(seqFilename).toString, "rw")
            try {
                try {
                    var n = seqfile.readLong + 1
                    while (Files.exists(getPath(n))) {
                        error(s"ChoiceData: $seqFilename is out of sync at $n")
                        n = n + 1
                    }
                    seqfile.seek(0L)
                    seqfile.writeLong(n)
                    n
                }
                finally {
                    seqfile.getChannel.force(true)
                }
            }
            finally { seqfile.close() }
        }
    }
    
    /**
     * Get a host file path for the next sequence-numbered file.
     */
    def nextFilename : Path = dataDir.resolve(getFileSeqNumber.toString + ".dat")
    
    private def setDataDir() : Unit = {
        lazy val catalinaBase = Box !! System.getProperty("catalina.base")

        def makeAbsPath(path : Path) : Box[Path] = {
            if (path.isAbsolute) Full(path) else {
                catalinaBase flatMap { cbase ⇒ tryo (Paths.get(cbase).resolve(path)) }
            }
        }

        // The following code determines where the DB filesystem will store file data
        // in the host filesystem. It first looks for a property, choice.HostDataFolder,
        // then a system parameter, choice.HostDataFolder. If both of those fail, it tries to
        // construct ${catalina.base}/ChoiceData.
        val dpath = (Props.get(HostDataFolder) flatMap { s ⇒
            val cleanpath = java.nio.file.Paths.get(s)
            val abspath = if (cleanpath.isAbsolute) Full(cleanpath) else makeAbsPath(cleanpath)
            abspath foreach (p ⇒ info(s"property $HostDataFolder is $p"))
            abspath
        }) or (Box !! System.getProperty(HostDataFolder) match {
            case Full(s) ⇒
                val cleanpath = java.nio.file.Paths.get(s)
                val abspath = if (cleanpath.isAbsolute) Full(cleanpath) else makeAbsPath(cleanpath)
                abspath foreach (p ⇒ info(s"system property for $HostDataFolder is $p"))
                abspath
            case _ : EmptyBox ⇒
                error(s"no system property for $HostDataFolder")
                Empty
        }) or (Box !! System.getProperty("catalina.base") match {
            case Full(s) ⇒
                val cleanpath = java.nio.file.Paths.get(s, "ChoiceData")
                info(s"using catalina.base to make host data folder $cleanpath")
                Full(cleanpath)
            case _ : EmptyBox ⇒
                error("catalina.base is not defined")
                Empty
        }) openOr null
                
        // Create the directory if it doesn't already exist
	    tryo {
            val dirfile = dpath.toFile
            dirfile.setWritable(true, true)
            dirfile.mkdirs
		    dataDir = dirfile.toPath
		    dataDir
	    } match {
            case Full(path) ⇒ info("ChoiceData dataDir is " + path.toString)
            case e : EmptyBox ⇒ error("Failed to create ChoiceData directory: " + dpath.toString, e)
        }
    }
}
