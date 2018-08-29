package choice

import _root_.java.io.File

import org.junit.jupiter.api.Assertions._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import org.junit.jupiter.api.Test

//object AppTest {
//  def suite: Unit = {
//    val suite = new TestSuite(classOf[AppTest])
//    suite
//  }
//
//  def main(args : Array[String]) {
//    _root_.junit.textui.TestRunner.run(suite)
//  }
//}

/**
 * Unit test for simple App.
 */
class AppTest {

  /**
   * Rigourous Tests :-)
   */
  @Test
  def testOK() : Unit = assertTrue(true)
  // def testKO() = assertTrue(false);

  /**
   * Tests to make sure the project's XML files are well-formed.
   *
   * Finds every *.html and *.xml file in src/main/webapp (and its
   * subdirectories) and tests to make sure they are well-formed.
   */
  @Test
  def testXml() : Unit = {
    var failed: List[File] = Nil

//    def handledXml(file: String) =
//      file.endsWith(".xml")

    def handledXHtml(file: String) : Boolean =
      file.endsWith(".html") || file.endsWith(".htm") || file.endsWith(".xhtml")

    def wellFormed(file: File) : Unit = {
      if (file.isDirectory) {
          if (file.getName != "library") {
              for (f : File ← file.listFiles) wellFormed(f)
          }
      }

      /*
      if (file.isFile && file.exists && handledXml(file.getName)) {
        try {
          import java.io.FileInputStream
          val fis = new FileInputStream(file)
          try {
            XML.load(fis)
          } finally {
            fis.close()
          }
        } catch {
          case e: _root_.org.xml.sax.SAXParseException => failed = file :: failed
        }
      }
      */

      if (file.isFile && file.exists && handledXHtml(file.getName)) {
        PCDataXmlParser(new _root_.java.io.FileInputStream(file.getAbsolutePath)) match {
          case Full(_) => // file is ok
          case f : Failure =>
              println(file.getName + " parse failed: " + f.msg)
              failed = file :: failed
          case _ =>
              println(file.getName + " parse returned Empty")
              failed = file :: failed
        }
      }
    }

    wellFormed(new File("src/main/webapp"))

    val numFails = failed.size
    if (numFails > 0) {
      val fileStr = if (numFails == 1) "file" else "files"
      val msg = "Malformed XML in " + numFails + " " + fileStr + ": " + failed.mkString(", ")
      println(msg)
      fail(msg)
    }
  }
}
