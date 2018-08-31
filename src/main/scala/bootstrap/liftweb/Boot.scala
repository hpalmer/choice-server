/**
  * Copyright © 2011-2017 The Board of Trustees of The Leland Stanford Junior University.
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
  * Lift Framework Bootstrap
  *
  * @author Howard Palmer
  */
package bootstrap.liftweb

import java.io.{File, FileInputStream, InputStream}
import java.nio.file.Paths
import java.util.Locale

import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import choice.core._
import choice.fs.ChoiceData
import net.liftweb.actor.ThreadPoolRules
import net.liftweb.common._
import net.liftweb.db.{DB, StandardDBVendor}
import net.liftweb.http._
import net.liftweb.http.provider.servlet.HTTPServletContext
import net.liftweb.util.Helpers._
import net.liftweb.util._
import org.slf4j.LoggerFactory

import scala.xml.NodeSeq

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
    import Boot.logger

    /**
      * When running under Tomcat, this should return the value of catalina.base if it's defined,
      * or otherwise the value of catalina.home. If neither is defined, this will be None, and
      * the assumption is that Tomcat is not the servlet container.
      */
    lazy val catalinaFolder : Option[String] = {
        Option(System.getProperty("catalina.base")) orElse Option(System.getProperty("catalina.home"))
    }

    /**
      * Try to get an input stream for a file in a folder.
      *
      * @param folderName the path to the folder
      * @param name the name of the file in the folder
      * @return a boxed InputStream, with a Failure if the file does not exist or cannot be read
      */
    def openFileInFolder(folderName : String, name : String) : Box[InputStream] = {
        tryo { new FileInputStream(new File(folderName, name)) } or Empty
    }

    def configureLogback() : Unit = {
        val lc = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
        val configurator = new JoranConfigurator()
        configurator.setContext(lc)

        def helper(name : String, xmlInput : InputStream) : Box[Unit] = {
            System.out.println(s"Configuring logback with: $name")
            tryo {
                lc.reset()
                configurator.doConfigure(xmlInput)
            }
        }

        // Use the same naming convention with respect to run.mode for logback.xml files
        // as Lift uses for property files.
        val logbackNames = Props.toTry map { f ⇒ s"${f()}logback.xml" }

        // Search first in a context-specific folder under ${catalina.base}/conf/Catalina/localhost
        val firstAttempt = catalinaFolder match {
            case Some(catalina) ⇒
                val contextPath = LiftRules.context.path.split('/')
                val relFolderPath = Seq("conf", "Catalina", "localhost") ++ contextPath
                val folderName = Paths.get(catalina, relFolderPath : _*).toString
                first(logbackNames) { name ⇒
                    val fullPath = Paths.get(folderName, name).toString
                    for {
                        xmlInput ← openFileInFolder(folderName, name)
                        result ← helper(fullPath, xmlInput)
                    } yield result
                }
            case None ⇒ Empty
        }

        // Failing that, look for a resource deployed via the .war file
        firstAttempt orElse first(logbackNames) { name ⇒
            for {
                url ← Box !! getClass.getResource(name)
                xmlInput ← tryo(url.openStream())
                result ← helper(name, xmlInput)
            } yield result
        }
    }

    def initializeDbVendor() : Unit = {
        // Running under Tomcat, it is preferred to configure the database as a JNDI resource.
        // If that has been done, no further action is needed here, since Lift can generate
        // a DB connection vendor from the JNDI name. When JNDI is used, Lift assumes that
        // the associated connection manager provides connection pooling, which Tomcat does.
        if (!DB.jndiJdbcConnAvailable_?) {
            // Fallback database configuration if the JNDI resource is not defined.
            // This uses a rudimentary connection pool provided by Lift.
            val driverBox = Props.get("db.driver")
            val urlBox = Props.get("db.url")
            val user = Props.get("db.user")
            val password = Props.get("db.password")

            (driverBox, urlBox) match {
                case (Full(driver), Full(url)) ⇒
                    val dbprops = Props.props.keys filter {
                        case n if !n.startsWith("db.") ⇒ false
                        case "db.driver" | "db.url" | "db.user" | "db.password" ⇒ false
                        case _ ⇒ true
                    }
                    // Add any additional db.* parameters to url query string
                    val urlAndQuery = dbprops.foldLeft(url) { (urlAcc, name) ⇒
                        val sname = name.substring(3)
                        Props.get(name) match {
                            case Full(s) ⇒
                                val sep = if (urlAcc contains '?') "&" else "?"
                                urlAcc + s"$sep$sname=$s"
                            case _ ⇒ urlAcc
                        }
                    }
                    val vendor = new StandardDBVendor(driver, urlAndQuery, user, password)
                    LiftRules.unloadHooks.append(() ⇒ vendor.closeAllConnections_!())
                    DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
                    // Try a connection. This will abort the servlet if it fails
                    vendor.newConnection(DefaultConnectionIdentifier) foreach vendor.releaseConnection
                case (Full(_), _) ⇒ throw new RuntimeException("missing db.url property")
                case (_, Full(_)) ⇒ throw new RuntimeException("missing db.driver property")
                case _ ⇒ throw new RuntimeException("missing db.driver and db.url properties")
            }
        }
    }

    def boot() : Unit = {

        // Make some system properties available for interpolation in our property files.
        val systemProperties = Seq(
            "catalina.base",
            "catalina.home"
        )
        val systemPList = systemProperties.foldLeft(Seq.empty[(String, String)]) { (seq, pname) ⇒
            Option(System.getProperty(pname)) match {
                case Some(pvalue) ⇒ seq :+ (pname → pvalue)
                case None ⇒ seq
            }
        }
        if (systemPList.nonEmpty) Props.appendInterpolationValues(Map(systemPList : _*))

        // Look for the Lift property files in catalina.base or else catalina.home. Specifically
        // we are looking for ${catalina.base}/conf/Catalina/localhost/${contextPath} as a folder
        // that might exist and contain property files. If a property file relevant to the current
        // run.mode is not found there, the usual places relative to the classpath are also searched.
        Props.whereToLook = catalinaFolder match {
            case Some(catalina) ⇒
                val contextPath = LiftRules.context.path.split('/')
                val relFolderPath = Seq("conf", "Catalina", "localhost") ++ contextPath
                val folderName = Paths.get(catalina, relFolderPath : _*).toString
                Props.appendInterpolationValues(Map("choice.ConfigFolder" → folderName))
                System.out.println(s"Searching for property files in $folderName")
                () ⇒ Props.toTry map { f ⇒
                    val name = s"${f()}props"
                    (Paths.get(folderName, name).toString, () ⇒ openFileInFolder(folderName, name))
                }
            case None ⇒ () ⇒ Nil
        }

        LiftRules.configureLogging = { () ⇒
            System.out.println("calling LoggingAutoConfigurer")
            LoggingAutoConfigurer()()
            configureLogback()
        }

        LiftRules.securityRules = () ⇒ SecurityRules(https = None, content = None)

        // Turn off X-Frame-Options: SAMEORIGIN
        val noXFrameOptions : List[(String, String)] = LiftRules.supplementalHeaders.vend.filterNot(_._1 == "X-Frame-Options")

        LiftRules.supplementalHeaders.default.set(() ⇒ noXFrameOptions)

        LiftRules.maxMimeSize = 4L * 1024L * 1024L * 1024L
        LiftRules.maxMimeFileSize = 4L * 1024L * 1024L * 1024L

        // Neither the client infrastructure nor the server routing is set up to handle
        // the session id as a URL parameter.
        LiftRules.encodeJSessionIdInUrl_? = false
        
        // Customize the name of the session cookie.
        //
        // Cookies are not port-specific, so two servers on the same host running on different
        // ports need a way to distinguish their session cookies. Here we customize the name
        // based on the low-order digits of the current time. Hopefully this will usually give
        // each server on a host a different session cookie name.
        //
        // If there were some way to reliably get the port number here, that would be a better
        // solution. However, as of Tomcat 7, that does not appear possible.
        //
        val jsessionName = LiftRules.context match {
            case c : HTTPServletContext ⇒
                val ccfg = c.ctx.getSessionCookieConfig
                val suffix = millis.toString.takeRight(5)
                val name = s"JSESSIONID$suffix"
                ccfg.setName(name)
                ccfg.setMaxAge(7 * 24 * 60 * 60)        // expire after a week
                name
        }

        // Lift's default headers turn off client caching. But if the file has been
        // cached by the server, the client can cache it too.
        // val defaultHeaders = LiftRules.defaultHeaders
        LiftRules.defaultHeaders = {
            case (_, _) ⇒ List(("Cache-Control", "no-cache,max-age=900"))
        }


        def myTemplateResolver : PartialFunction[(Locale, List[String]), Box[NodeSeq]] = {
            case (_ : Locale, list : List[String]) if list.last != "default" ⇒
                logger.error(s"""myTemplateResolver: ${list.mkString(", ")}""")
                Empty
        }

        LiftRules.externalTemplateResolver.default.set(Vendor(() ⇒ myTemplateResolver _))
        ChoiceData.init

        // This is needed to keep JNDI lookup working in worker threads
        ThreadPoolRules.nullContextClassLoader = false

        initializeDbVendor()

        // where to search snippet
        LiftRules.addToPackages("choice")

        Startup.boot(jsessionName)

        LiftRules.dispatch.append(Component.resolveRequest)

        // Use HTML5 responses
        LiftRules.htmlProperties.default.set((r: Req) ⇒ Html5Properties(r.userAgent))

        // No XML header, i.e. no <?xml version="1.0" encoding="utf-8"?>,
        //     as IE6 goes onto quirks mode seeing this
        LiftRules.calculateXmlHeader = (_, _, _) ⇒ ""

        def myProgressListener(read : Long, total : Long, nitem : Int) : Unit = {
            tryo {
                logger.info("read " + read + " bytes of " + total + ", in " + nitem + " chunks")
                S.request match {
                    case Full(req) ⇒
                        logger.info("progressListener sees request: " + req.sessionId)
                        val liftSession = LiftRules.getLiftSession(req)
                        logger.info("progressListener got LiftSession for " + liftSession.uniqueId)
                    case Empty ⇒ logger.info("progressListener sees no request")
                    case _ : Failure ⇒ logger.error("progressListener request check")
                }
                read
            } match {
                case Full(_) ⇒
                case e : EmptyBox ⇒ logger.error("progressListener error", e)
            }
        }
        LiftRules.progressListener = myProgressListener

        object SuffixExtractor {
            def unapply(path : List[String]) : Option[(List[String], String)] = {
                if (path == Nil) None
                else {
                    val last = path.last
                    val i = last.lastIndexOf('.')
                    if (i >= 0) {
                        val (fpart, suffix) = last.splitAt(i)
                        Some((path.dropRight(1) ::: List(fpart), suffix.substring(1)))
                    }
                    else None
                }
            }
        }

        LiftRules.suffixSplitters.prepend({
            case SuffixExtractor(path, suffix) ⇒ (path, suffix)
        })

//        LiftRules.liftRequest.append {
//            case Req("monitoring" :: _, _, GetRequest) ⇒ false      // For JavaMelody
//            case _ ⇒ true
//        }

        LiftRules.passNotFoundToChain = true

        SiteMap.enforceUniqueLinks = false

        LiftRules.autoIncludeAjaxCalc.default.set(Vendor(() ⇒ (_ : LiftSession) ⇒ false))
        LiftRules.autoIncludeComet = (_ : LiftSession) ⇒ false
        LiftRules.enableLiftGC = false
        LiftRules.javaScriptSettings.default.set(Vendor(() ⇒ Empty))

        /*
         * Show the spinny image when an Ajax call starts
         */
        LiftRules.ajaxStart = Full(() ⇒ LiftRules.jsArtifacts.show("ajax-loader").cmd)

        /*
         * Make the spinny image go away when it ends
         */
        LiftRules.ajaxEnd = Full(() ⇒ LiftRules.jsArtifacts.hide("ajax-loader").cmd)

        LiftRules.early.append(makeUtf8)
    }

    /**
      * Force the request to be UTF-8
      */
    private def makeUtf8(req : HTTPRequest) : Unit = {
        req.setCharacterEncoding("UTF-8")
    }
}

object Boot {
    lazy val logger = Logger("bootstrap.liftweb.Boot")

    def getDefaultLogger : Logger = logger
}