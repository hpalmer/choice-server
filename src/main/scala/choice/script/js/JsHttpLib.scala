/**
  * Copyright © 2017 The Board of Trustees of The Leland Stanford Junior University.
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
  * Created by Hep on 1/2/2017.
  */
package choice.script.js

import javax.script.ScriptEngine

import choice.access.Principal
import choice.script.HttpLib
import jdk.nashorn.api.scripting.JSObject
import net.liftweb.common.{EmptyBox, Full}

class JsHttpLib(private val context : JavaScriptContext, private implicit val engine : ScriptEngine) {

    private def httpLib : HttpLib = context.httpLib

    private def principal : Principal = context.principal

    /**
      * Return the context part of the URL path, e.g. "/fschoice". Presumably, if the
      * server were running in the root context, this would return "/".
      *
      * @return the context string
      */
    def getContextPath : String = httpLib.getContextPath

    /**
      * Return the host and context path of the request URL, e.g.
      *     "http://localhost:8888/fschoice"
      *
      * @return the URL prefix string
      */
    def getHostAndPath : String = httpLib.getHostAndPath

    /**
      * Return just the part of the URL up through the context, e.g.
      *     "http://localhost:8888/fschoice"
      *
      * @return the URL prefix string
      */
    def getURI : String = httpLib.getURI

    /**
      * Return the request headers as a list of (name, value) pairs.
      *
      * @return the list
      */
    def getHeaders : Array[Array[String]] = httpLib.getHeaders.toArray map { pair ⇒
        Array(pair._1, pair._2)
    }

    /**
      * Get the values of a named request header as a list.
      *
      * @param name the name of the header to retrieve
      * @return a list of values for the header
      */
    def getHeader(name : String) : Array[String] = httpLib.getHeader(name).toArray

    /**
      * Get the URL query parameters as a map of query parameter name to a list of
      * values for the parameter.
      *
      * @return the map
      */
    def getQueryParams : JSObject = {
        val pmap = httpLib.getQueryParams
        val jo = Launcher.makeObject()
        for (pname ← pmap.keys) {
            val v = pmap.getOrElse(pname, Nil)
            jo setMember (pname, Launcher.listForJS(v))
        }
        jo
    }

    /**
      * Return a value for a specified query parameter. If the query parameter
      * may have multiple values, use getQueryParams.
      *
      * @param name the query parameter name
      * @return the query parameter value or null
      */
    def getQueryParam(name : String) : String = httpLib.getQueryParam(name) match {
        case Full(v) ⇒ v
        case _ : EmptyBox ⇒ null
    }
}
