/**
  * Copyright © 2016-2017 The Board of Trustees of The Leland Stanford Junior University.
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
package choice.lib

import java.math.BigInteger
import java.net.{HttpURLConnection, URL, URLEncoder}
import java.nio.charset.StandardCharsets
import java.security.spec.RSAPublicKeySpec
import java.security.{KeyFactory, PublicKey, Signature}
import java.util.Base64

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.json.JsonAST.{JArray, JNothing, JObject}
import net.liftweb.json.{DefaultFormats, Extraction, JsonParser}
import net.liftweb.util.Props
import net.liftweb.util.Helpers.millis

import scala.io.Source


object OAuthHelpers {

    implicit val format : DefaultFormats.type = DefaultFormats
    private val rsaKeyFactory = KeyFactory.getInstance("RSA")
    private lazy val googleCredentials = {
        val boxed = Props.get("google.client.id") flatMap { id ⇒
            Props.get("google.client.secret") map (id → _)
        }
        boxed.toOption
    }
    private val googleTokenEndpoint = "https://www.googleapis.com/oauth2/v4/token"

    sealed case class RSAKeyObject(kid : String, e : String, n : String) {
        def getExponent : BigInteger = {
            val decoder = Base64.getUrlDecoder
            new BigInteger(1, decoder.decode(e))
        }

        def getModulus : BigInteger = {
            val decoder = Base64.getUrlDecoder
            new BigInteger(1, decoder.decode(n))
        }
    }

    case class WebToken(header : Map[String, Any], claims : Map[String, Any])

    def parseWebToken(token : String, pubkeys : Map[String, PublicKey]) : Box[WebToken] = {
        val parts = token.split('.')
        if (parts.length != 3) Failure(s"token has ${parts.length} parts instead of 3")
        else {
            val jvhdr = JsonParser.parseOpt(new String(Base64.getUrlDecoder.decode(parts(0))))
            val jvclaims = JsonParser.parseOpt(new String(Base64.getUrlDecoder.decode(parts(1))))
            val sigbytes = Base64.getUrlDecoder.decode(parts(2))
            (jvhdr, jvclaims) match {
                case (Some(hdrobj : JObject), Some(clobj : JObject)) ⇒
                    val hdrmap = hdrobj.values
                    val clmap = clobj.values
                    if (hdrmap.getOrElse("alg", "RS256") != "RS256") Failure("unsupported web token signature")
                    else {
                        // Try to match a key id ("kid") value in the token header with a key in pubkeys
                        val pkopt = hdrmap.get("kid") match {
                            case Some(kid : String) ⇒ pubkeys.get(kid)
                            case Some(kid : BigInt) ⇒ pubkeys.get(kid.toString)
                            case Some(_) ⇒ None
                            case None ⇒ None
                        }
                        // If that didn't work, but there is only one key in pubkeys, use it
                        val pkopt2 = pkopt match {
                            case pk @ Some(_) ⇒ pk
                            case None ⇒
                                if (pubkeys.size != 1) None
                                else Some(pubkeys.head._2)
                        }
                        // If we managed to come up with a key, use it to check the signature
                        val goodSig = pkopt2 match {
                            case Some(pkey) ⇒
                                val sig = Signature.getInstance("SHA256withRSA")
                                sig.initVerify(pkey)
                                sig.update(s"${parts(0)}.${parts(1)}".getBytes(StandardCharsets.UTF_8))
                                if (sig.verify(sigbytes)) Full(WebToken(hdrmap, clmap))
                                else Failure(s"web token signature validation failed")
                            case None ⇒ Failure("unable to identify the signing key")
                        }
                        goodSig flatMap { wtoken ⇒
                            val iss = wtoken.claims.getOrElse("iss", "")
                            val aud = wtoken.claims.getOrElse("aud", "")
                            val exp = wtoken.claims.get("exp") match {
                                case Some(bi : BigInt) ⇒ bi.toLong * 1000
                                case _ ⇒ 0L
                            }
                            // Check the validity of the iss, aud, and exp fields
                            if ((iss == "accounts.google.com" || iss == "https://accounts.google.com") &&
                                googleCredentials.exists(_._1 == aud) && (exp > millis)) Full(wtoken)
                            else Failure("Invalid web token")
                        }
                    }
                case _ ⇒ Failure("malformed web token")
            }
        }
    }

    def getGoogleAccessToken(code : String) : Box[Map[String, Any]] = {
        googleCredentials match {
            case Some((client_id, client_secret)) ⇒
                val reqmap = Map(
                    "code" → code,
                    "client_id" → client_id,
                    "client_secret" → client_secret,
                    "redirect_uri" → "http://localhost:8888/fschoice/googleauth?api=user",
                    "grant_type" → "authorization_code"
                )
                val token_endpoint = new URL(googleTokenEndpoint)
                val reqdata = new StringBuilder(code.length + client_id.length + client_secret.length + 256)
                for ((key, value) ← reqmap) {
                    if (reqdata.nonEmpty) {
                        reqdata.append("&")
                    }
                    reqdata.append(key)
                    reqdata.append("=")
                    reqdata.append(URLEncoder.encode(value, "UTF-8"))
                }
                val reqbytes = reqdata.mkString.getBytes(StandardCharsets.UTF_8)
                try {
                    val conn = token_endpoint.openConnection().asInstanceOf[HttpURLConnection]
                    conn.setDoOutput(true)
                    conn.setInstanceFollowRedirects(false)
                    conn.setRequestMethod("POST")
                    conn.setRequestProperty("Content-Type", "application/x-www-form-urlencoded")
                    conn.setRequestProperty("charset", "utf-8")
                    conn.setRequestProperty("Content-Length", reqbytes.length.toString)
                    conn.setUseCaches(false)
                    conn.getOutputStream.write(reqbytes)
                    val response = Source.fromInputStream(conn.getInputStream, "UTF-8").mkString
                    val result = Extraction.extract[Map[String, Any]](JsonParser.parse(response))
                    Full(result)
                }
                catch {
                    case ex : Exception ⇒ Failure("exception in getGoogleAccessToken", Full(ex), Empty)
                }
            case None ⇒ Failure("missing client credentials for Google")
        }
    }

    def getGooglePublicKeys : Box[Map[String, PublicKey]] = fetchRSAKeys("https://www.googleapis.com/oauth2/v3/certs")

    def fetchRSAKeys(url : String) : Box[Map[String, PublicKey]] = {
        try {
            JsonParser.parseOpt(Source.fromURL(url).reader(), closeAutomatically = true) match {
                case Some(jval : JObject) ⇒
                    val keys = jval \ "keys"
                    if (keys == JNothing) {
                        // No "keys" field, so assume it's just one key
                        val obj = Extraction.extract[RSAKeyObject](jval)
                        Full(Map(obj.kid →
                            rsaKeyFactory.generatePublic(new RSAPublicKeySpec(obj.getModulus, obj.getExponent))))
                    }
                    else if (keys.isInstanceOf[JArray]) {
                        // If there is a "keys" field, it better be an array of keys
                        val pairs = Extraction.extract[Array[RSAKeyObject]](keys) map { obj ⇒
                            obj.kid → rsaKeyFactory.generatePublic(new RSAPublicKeySpec(obj.getModulus, obj.getExponent))
                        }
                        Full(Map(pairs : _*))
                    }
                    else Failure("object contains 'keys', but value is not an array")
                case Some(keys : JArray) ⇒
                    // If it's already an array, assume it's an array of keys
                    val pairs = Extraction.extract[Array[RSAKeyObject]](keys) map { obj ⇒
                        obj.kid → rsaKeyFactory.generatePublic(new RSAPublicKeySpec(obj.getModulus, obj.getExponent))
                    }
                    Full(Map(pairs : _*))
                case Some(_) ⇒ Failure("RSA keys JSON is not an object or array")
                case None ⇒ Failure("unable to parse JSON for RSA keys")
            }
        }
        catch {
            case ex : Exception ⇒ Failure("exception in fetchRSAKeys", Full(ex), Empty)
        }
    }
}
