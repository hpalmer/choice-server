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
package choice.script

import choice.access.Principal
import net.liftweb.common.Box
import net.liftweb.http.Req

/**
  * Provide server scripts with access to the HTTP request which caused their execution.
  * Perhaps someday this will also enable scripts to construct the HTTP response directly.
  *
  * @param req the Lift request object
  * @param principal a function which returns the Principal associated with the running
  *                  script
  */
class HttpLib(private val req : Req, private val principal : () ⇒ Principal) {

    /**
      * Return the context part of the URL path, e.g. "/fschoice". Presumably, if the
      * server were running in the root context, this would return "/".
      *
      * @return the context string
      */
    def getContextPath : String = req.contextPath

    /**
      * Return the host and context path of the request URL, e.g.
      *     "http://localhost:8888/fschoice"
      *
      * @return the URL prefix string
      */
    def getHostAndPath : String = req.hostAndPath

    /**
      * Return path part of the URL which follows the context path. So for example,
      * a URL of "http://localhost:8888/fschoice/home/howard/info.html" would result
      * in "/home/howard/info.html".
      *
      * @return the URI path string
      */
    def getURI : String = req.uri

    /**
      * Return the request headers as a list of (name, value) pairs.
      *
      * @return the list
      */
    def getHeaders : List[(String, String)] = req.headers

    /**
      * Get the values of a named request header as a list.
      *
      * @param name the name of the header to retrieve
      * @return a list of values for the header
      */
    def getHeader(name : String) : List[String] = req.headers(name)

    /**
      * Get the URL query parameters as a map of query parameter name to a list of
      * values for the parameter.
      *
      * @return the map
      */
    def getQueryParams : Map[String, List[String]] = req.params

    /**
      * Return a value for a specified query parameter. If the query parameter
      * may have multiple values, use getQueryParams.
      *
      * @param name the query parameter name
      * @return the boxed query parameter value
      */
    def getQueryParam(name : String) : Box[String] = req.param(name)
}
