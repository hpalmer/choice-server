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
import javax.naming.{Context, InitialContext}
import net.liftweb.actor.ThreadPoolRules
import net.liftweb.common._
import net.liftweb.db.{DB, StandardDBVendor}
import net.liftweb.http._
import net.liftweb.http.provider.servlet.HTTPServletContext
import net.liftweb.util.Helpers._
import net.liftweb.util._
import org.slf4j.LoggerFactory
import org.apache.tomcat.jdbc.pool.{DataSource, PoolProperties}

import scala.xml.NodeSeq

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
    import Boot.logger

    lazy val catalinaFolder : Option[String] = {
        Option(System.getProperty("catalina.base")) orElse Option(System.getProperty("catalina.home"))
    }

    def openTomcatFile(folderName : String, name : String) : Box[InputStream] = {
        tryo { new FileInputStream(new File(folderName, name)) } or Empty
    }

    def configureLogback() : Unit = {
        val lc = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
        val configurator = new JoranConfigurator()
        configurator.setContext(lc)

        val logbackNames = Props.toTry map { f ⇒ s"${f()}logback.xml" }
        val firstAttempt = catalinaFolder.map(Paths.get(_, "conf").toString) match {
            case Some(folderName) ⇒
                first(logbackNames map { name ⇒
                    (Paths.get(folderName, name).toString, () ⇒ openTomcatFile(folderName, name))
                }) {
                    case (name, f) ⇒
                        f() flatMap { instream ⇒
                            System.out.println(s"Configuring logback with: $name")
                            tryo {
                                lc.reset()
                                configurator.doConfigure(instream)
                            }
                        }
                }
            case None ⇒ Empty
        }

        firstAttempt or {
            first(logbackNames map { name ⇒
                (name, () ⇒ {
                    Box.legacyNullTest(getClass.getResource(name))
                })
            }) {
                case (name, f) ⇒
                    f() flatMap { url ⇒
                        System.out.println(s"Configuring logback with: $name")
                        tryo {
                            lc.reset()
                            configurator.doConfigure(url)
                        }
                    }
            }
        }
    }

    def initializeDbVendor() : Unit = {
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
            case (Full(_), _) ⇒ throw new RuntimeException("missing db.url property")
            case (_, Full(_)) ⇒ throw new RuntimeException("missing db.driver property")
            case _ ⇒ throw new RuntimeException("missing db.driver and db.url properties")
        }
    }

    def initializeDataSourceVendor() : Unit = {
        if (!DB.jndiJdbcConnAvailable_?) {
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
                    val poolProperties = new PoolProperties()
                    poolProperties.setDriverClassName(driver)
                    poolProperties.setUrl(urlAndQuery)
                    user foreach poolProperties.setUsername
                    password foreach poolProperties.setPassword
                    Props.get("dbpool.jdbcInterceptors") foreach poolProperties.setJdbcInterceptors
                    poolProperties.setLogAbandoned(Props.getBool("dbpool.logAbandoned", defVal = true))
                    poolProperties.setMaxActive(Props.getInt("dbpool.maxActive", 16))
                    poolProperties.setMaxIdle(Props.getInt("dbpool.maxIdle", 8))
                    poolProperties.setMaxWait(Props.getInt("dbpool.maxWait", 30000))
                    poolProperties.setMinIdle(Props.getInt("dbpool.minIdle", 4))
                    poolProperties.setRemoveAbandoned(Props.getBool("dbpool.removeAbandoned", defVal = true))
                    poolProperties.setRemoveAbandonedTimeout(Props.getInt("dbpool.removeAbandonedTimeout", 60))
                    poolProperties.setTestOnBorrow(Props.getBool("dbpool.testOnBorrow", defVal = true))
                    poolProperties.setTestOnReturn(Props.getBool("dbpool.testOnReturn", defVal = false))
                    poolProperties.setTestWhileIdle(Props.getBool("dbpool.testWhileIdle", defVal = true))
                    poolProperties.setValidationInterval(Props.getLong("dbpool.validationInterval", 3000))
                    Props.get("dbpool.validationQuery") foreach poolProperties.setValidationQuery
                    val dsource = new DataSource(poolProperties)
                    val cipath = DefaultConnectionIdentifier.jndiName.split('/').dropWhile(_ == "")
                    val ic = new InitialContext()
                    val lastctx = {
                        if (cipath.length > 1) {
                            cipath.dropRight(1).foldLeft(ic : Context) { (nctx, name) ⇒ nctx.createSubcontext(name) }
                        }
                        else ic
                    }
                    lastctx.bind(cipath.last, dsource)
                case (Full(_), _) ⇒ throw new RuntimeException("missing db.url property")
                case (_, Full(_)) ⇒ throw new RuntimeException("missing db.driver property")
                case _ ⇒ throw new RuntimeException("missing db.driver and db.url properties")
            }
        }
    }

    def boot() : Unit = {

        // Look for the Lift property files in catalina.base or else catalina.home. If not found
        // there, the usual places relative to the classpath are also searched.
        Props.whereToLook = catalinaFolder.map(Paths.get(_, "conf").toString) match {
            case Some(folderName) ⇒
                System.out.println(s"Searching for property files in $folderName")
                () ⇒ Props.toTry map { f ⇒
                    val name = s"${f()}props"
                    (Paths.get(folderName, name).toString, () ⇒ openTomcatFile(folderName, name))
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

        // initializeDbVendor()
        initializeDataSourceVendor()

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