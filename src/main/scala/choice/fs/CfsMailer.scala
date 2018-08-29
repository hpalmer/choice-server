/**
  * Copyright © 2016 The Board of Trustees of The Leland Stanford Junior University.
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

import java.util.Properties
import javax.mail.Message.RecipientType
import javax.mail.internet.{InternetAddress, MimeMessage}
import javax.mail._

import choice.access.{AnyRightsCheck, Principal, RightDef, RightsCheck}
import choice.model.Resource
import net.liftweb.common._
import net.liftweb.json.DefaultFormats
import net.liftweb.mapper.BaseMetaMapper
import net.liftweb.util.Helpers.tryo

case class MailerMessage(from : Option[String],
                         to : List[String],
                         cc: List[String],
                         bcc: List[String],
                         subject : Option[String],
                         headers : Option[Map[String, String]],
                         html : Boolean = false,
                         content : String)

case class MailerSettings(host : String,
                          port : Option[Int],
                          auth : Boolean,
                          starttls : Boolean,
                          username : Option[String],
                          password : Option[String])

class MailerCreateOptions(val settings : MailerSettings,
                          override val dataSeqnum : Long, override val ctime : Long,
                          override val altid : Long, override val replace : Boolean)
    extends CfsCreateOptions(dataSeqnum = dataSeqnum, ctime = ctime, altid = altid, replace = replace)

class MailerNode(resource : Resource) extends CfsSpecialNode(resource)
    with AtomicDataNode[MailerNode]
    with JsonDataNode[MailerNode, MailerSettings] {
    val mf : Manifest[MailerSettings] = manifest[MailerSettings]

    /**
      * Open the file associated with this Vnode. Mainly that means creating
      * a CfsFile that references this CfsVnode. Using the CfsFile,
      * the principal can perform various operations on the file. Depending
      * on the MIME type of the file, that may include acquiring input or
      * output streams, or performing other operations that are specific to
      * the file type.
      *
      * This method is mainly for a subclass of CfsVnode to return a corresponding
      * subclass of CfsFile, though it may do other things. Common open() processing
      * should be done in CfsVnode open().
      *
      * The reference count of this CfsVnode is assumed to have already been
      * incremented prior to calling its cfsOpen() method. If cfsOpen() is
      * successful, the reference count will be decremented when the returned
      * CfsFile handle is closed. If cfsOpen() fails, the caller (usually
      * CfsVnode open()) is responsible for decrementing the reference count.
      *
      * @param path      the path that led to this CfsVnode
      * @param principal the principal to be associated with the CfsFile
      * @param options   filesystem and file type specific options that may
      *                  affect the state of the resulting CfsFile
      * @return a boxed CfsFile if successful, otherwise a Failure. Empty
      *         should not be returned.
      */
    override def cfsOpen(path : CfsAbsolutePath, principal : Principal, options : CfsOpenOptions) : Box[CfsFile] = {
        Full(new CfsMailer(path, principal, this))
    }

    /**
      * Return true if this file is a container, i.e. supports the lookup() operation.
      * All Cfs files are containers, even if only for metadata files.
      *
      * @return true if this Vnode represents a container
      */
    override def isContainer_? : Boolean = false

    /**
      * Subclasses override this function in order to do custom file creation processing.
      * That may include processing create options which are specific to the file type.
      * The subclass also has the option to return a different Vnode than the current one.
      * If it does, the current Vnode should be released, and the returned Vnode should be
      * acquired.
      *
      * @param name      the requested name of the member (which may be changed later via an
      *                  overload of the acceptName_? method)
      * @param principal the principal requesting the file creation
      * @param options   options for the file creation, possibly file type specific
      * @return a boxed CfsVnode (usually this one) if successful, otherwise Failure
      */
    override def cfsCreate(name : String, principal : Principal, options : CfsCreateOptions) : Box[CfsVnode] = {
        options match {
            case mailerOpt : MailerCreateOptions ⇒
                val valid = mailerOpt.settings match {
                    case MailerSettings(_, _, false, _, _, _) ⇒ true
                    case MailerSettings(_, _, true, _, Some(_), Some(_)) ⇒ true
                    case _ ⇒ false
                }
                if (valid) {
                    putData(mailerOpt.settings)
                }
                else Failure(s"authenticating mailer '$name' is missing username and/or password")
            case _ ⇒ Failure(s"missing mailer creation options for user '$name'")
        }
    }
}

/**
  * Created by Hep on 8/3/2016.
  */
class CfsMailer(path : CfsAbsolutePath, principal : Principal, vnode : MailerNode)
    extends CfsSpecial(path, principal, vnode)
    with JsonDataFile[CfsMailer, MailerNode, MailerSettings] {
    implicit val formats = DefaultFormats

    override def getData : Box[MailerSettings] = (CfsMailer canReadSettings this) { () ⇒
        getDataUnchecked
    }

    override def putData(data : MailerSettings) : Box[Boolean] = (CfsMailer canWriteSettings this) { () ⇒
        putDataUnchecked(data) map (_ ⇒ true)
    }

    def send(message : MailerMessage) : Box[Boolean] = (CfsMailer canSendEmail this) { () ⇒
        getDataUnchecked flatMap { settings ⇒
            val props = new Properties()
            props.setProperty("mail.host", settings.host)
            settings.port.foreach { port ⇒
                props.setProperty("mail.smtp.port", port.toString)
                props.setProperty("mail.smtp.socketFactory.port", port.toString)
            }
            settings.username.foreach(props.setProperty("mail.user", _))
            if (settings.starttls) {
                props.setProperty("mail.smtp.starttls.enable", "true")
            }
            tryo[Boolean] {
                val session =
                    if (settings.auth) {
                        props.setProperty("mail.smtp.auth", "true")
                        Session.getInstance(props, new Authenticator() {
                            override def getPasswordAuthentication : PasswordAuthentication = {
                                new PasswordAuthentication(settings.username.get, settings.password.get)
                            }
                        })
                    }
                    else {
                        props.setProperty("mail.smtp.auth", "false")
                        Session.getInstance(props)
                    }
                val msg = new MimeMessage(session)
                // If the message does not include a 'from' address, use the relay host username
                val fromAddress = message.from.getOrElse {
                    settings.username match {
                        case Some(username) ⇒
                            // If the relay host username doesn't include a host part,
                            // append the relay host as the host part
                            if (username contains "@") username
                            else s"$username@${settings.host}"
                        case None ⇒
                            // There is no relay username, so probably no one cares
                            s"anonymous@${settings.host}"
                    }
                }
                msg.addFrom(Array(new InternetAddress(fromAddress)))
                message.subject.foreach(msg.setSubject)
                msg.addRecipients(RecipientType.TO, message.to.map(to ⇒
                    new InternetAddress(to).asInstanceOf[Address]).toArray)
                msg.addRecipients(RecipientType.CC, message.cc.map(cc ⇒
                    new InternetAddress(cc).asInstanceOf[Address]).toArray)
                msg.addRecipients(RecipientType.BCC, message.bcc.map(bcc ⇒
                    new InternetAddress(bcc).asInstanceOf[Address]).toArray)
                msg.setText(message.content, "UTF-8", if (message.html) "html" else "plain")
                val tpname = if (settings.starttls) "smtps" else "smtp"
                val transport = session.getTransport(tpname)
                if (settings.auth) transport.connect(settings.username.get, settings.password.get)
                else transport.connect()
                transport.sendMessage(msg, msg.getAllRecipients)
                transport.close()
                true
            }
        }
    }

    override def asMap : Map[String, Any] = {
        this.getData match {
            case Full(settings) ⇒
                Map("host" → settings.host, "port" → settings.port.getOrElse(25),
                    "auth" → settings.auth, "starttls" → settings.starttls,
                    "username" → settings.username) ++ super.asMap
            case _ : EmptyBox ⇒ super.asMap
        }
    }
}

object CfsMailer extends MimeTypeHandler {

    override protected def Log : Logger = Logger("choice.fs.CfsMailer")

    override def getName : String = "Mailer File Type"

    /**
      * Get a list of schemas used by this module, to be included in the Boot
      * schemify step.
      *
      * @return a list of Mapper objects representing database tables
      */
    override def getSchemas : List[BaseMetaMapper] = Nil

    /**
      * Return a list of rights definitions for this module.
      *
      * @return list of rights definitions
      */
    override def getRights : List[RightDef] = List(
        RightDef("create_mailer", "mailer object", "create a mailer"),
        RightDef("unlink_mailer", "mailer object", "unlink a mailer"),
        RightDef("read_mailer", "mailer object", "read mailer settings"),
        RightDef("write_mailer", "mailer object", "write mailer settings"),
        RightDef("send_email", "mailer object", "send an email")
    )

    /**
      * Get the string representation of the MIME type associated with this handler.
      */
    override def getMimeType : String = "choice/mailer"

    /**
      * Wrap a Resource object in an instance of the HandlerType for this MIME type handler.
      *
      * @param resource the Resource object as stored in the DB
      * @return if the resource is compatible with this MIME type, a boxed wrapper for the
      *         given resource, otherwise Failure
      */
    override def instantiate(resource : Resource) : Box[CfsVnode] = {
        if (isType_?(resource)) Full(new MailerNode(resource))
        else Failure(s"CfsMailer instantiate passed a non-mailer resource")
    }

    /**
      * This defines the rights needed to create a new instance of this object type.
      * The principal must hold this right for the container in which the object is
      * to be created.
      *
      * @return a RightsCheck instance
      */
    override def canCreateObject : RightsCheck = AnyRightsCheck("create_mailer")

    /**
      * This defines the rights needed to unlink an object of this type.
      * The principal must hold this right for the container from which the object is
      * to be unlinked.
      *
      * @return a RightsCheck instance
      */
    override def canUnlinkObject : RightsCheck = AnyRightsCheck("unlink_mailer")

    val canReadSettings = AnyRightsCheck("read_mailer")
    val canWriteSettings = AnyRightsCheck("write_mailer")
    val canSendEmail = AnyRightsCheck("send_email")
}