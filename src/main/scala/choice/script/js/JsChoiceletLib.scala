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
  * Created by Hep on 7/12/2017.
  */
package choice.script.js

import javax.script.{ScriptEngine, ScriptException}

import choice.access.Principal
import choice.script._
import jdk.nashorn.api.scripting.JSObject
import net.liftweb.common.{EmptyBox, Full}

class JsChoiceletLib(private val context : JavaScriptContext, private implicit val engine : ScriptEngine) {

    private var _openedChoicelets = List.empty[ChoiceletHomeHandle]

    private def cfsFileLib = context.cfsFileLib

    private def choiceletLib = context.choiceletLib

    private def principal : Principal = context.principal

    private def choiceletClosing(chhome : ChoiceletHomeHandle) : Unit = {
        _openedChoicelets = _openedChoicelets filterNot (_ eq chhome)
    }

    def closeAll() : Unit = {
        _openedChoicelets foreach (_ close ())
        _openedChoicelets = Nil
    }

    def getChoiceletHome(name : String, version : String) : ChoiceletHomeHandle = {
        choiceletLib.getChoiceletHome(name, version) match {
            case Full(chhome) ⇒
                _openedChoicelets = chhome :: _openedChoicelets
                chhome.onClose(choiceletClosing)
            case e : EmptyBox ⇒
                val errmsg = (e ?~ "Empty").msg
                throw new ScriptException(s"Choicelet $name version $version: $errmsg")
        }
    }

    def getChoiceletHome(id : Long) : ChoiceletHomeHandle = {
        choiceletLib.getChoiceletHome(id) match {
            case Full(chhome) ⇒
                _openedChoicelets = chhome :: _openedChoicelets
                chhome.onClose(choiceletClosing)
            case e : EmptyBox ⇒
                val errmsg = (e ?~ "Empty").msg
                throw new ScriptException(s"Choicelet id $id: $errmsg")
        }
    }

    def findOrCreateChoiceletHome(name : String, version : String) : ChoiceletHomeHandle = {
        choiceletLib.findOrCreateChoiceletHome(name, version) match {
            case Full(chhome) ⇒
                _openedChoicelets = chhome :: _openedChoicelets
                chhome.onClose(choiceletClosing)
            case e : EmptyBox ⇒
                val errmsg = (e ?~ "Empty").msg
                throw new ScriptException(s"Choicelet $name version $version: $errmsg")
        }
    }

    /**
      * Get information about Choicelets which match a given query. Note that the
      * objects in the returned list must be closed by the caller.
      *
      * @param query specifies characteristics of the Choicelets of interest
      * @return a list of Choicelet home handles for matching Choicelets
      */
    def getChoicelets(query : JSObject /* ChoiceletSpec */) : Array[ChoiceletHomeHandle] = {
        val id = Launcher.optLongField(query, "id")
        val client = Launcher.optLongField(query, "client")
        val name = Launcher.optStringField(query, "name")
        val version = Launcher.optStringField(query, "version")
        val result = choiceletLib.getChoicelets(ChoiceletSpec(id, name, version, client)).toArray
        result foreach (_.onClose(choiceletClosing))
        result
    }

    def findChoiceletVersions(name : String) : Array[ChoiceletHomeHandle] = {
        val result = choiceletLib.findChoiceletVersions(name).toArray
        result foreach (_.onClose(choiceletClosing))
        result
    }

    def getChoiceletStateById(client : Long, state : Long) : ChoiceletState = {
        choiceletLib.getExistingStateInfo(state) match {
            case Full((_, chstate)) if chstate.client == client ⇒ chstate
            case Full(_) ⇒ throw new ScriptException(s"Choicelet id $client has no state with id $state")
            case e : EmptyBox ⇒
                val errmsg = (e ?~ "Empty").msg
                throw new ScriptException(s"Choicelet id $client, state id $state: $errmsg")
        }
    }

    def makeChoiceletSummarySpec(query : JSObject) : ChoiceletSummarySpec = {
        val client = Launcher.optLongField(query, "client")
        val name = Launcher.optStringField(query, "name")
        val version = Launcher.optStringField(query, "version")
        val id = Launcher.optLongField(query, "id")
        val closed = Launcher.optBooleanField(query, "closed")
        val deluxe = Launcher.optBooleanField(query, "deluxe")
        val gid = Launcher.optLongField(query, "gid")
        val group = Launcher.optStringField(query, "group")
        val from = Launcher.optLongField(query, "from")
        val until = Launcher.optLongField(query, "until")
        ChoiceletSummarySpec(name, version, client, id, closed, deluxe, group, gid, from, until)
    }

    def getChoiceletSummary(query : JSObject /* ChoiceletSummarySpec */) : Array[JSObject] = {
        (choiceletLib.getChoiceletSummary(makeChoiceletSummarySpec(query)) map Launcher.mapToObject).toArray
    }

    def getStateSummary(query : JSObject /* ChoiceletSummarySpec */) : Array[JSObject] = {
        (choiceletLib.getStateSummary(makeChoiceletSummarySpec(query)) map Launcher.mapToObject).toArray
    }

    def findState(query : JSObject /* ChoiceletSummarySpec */) : Array[JSObject] = {
        choiceletLib.findState(makeChoiceletSummarySpec(query)) match {
            case Full(maps) ⇒ maps map Launcher.mapToObject
            case e : EmptyBox ⇒
                val errmsg = (e ?~ "Empty").msg
                throw new ScriptException(errmsg)
        }
    }

    def getDeluxe(client : Long, id : Long) : JSObject = {
        choiceletLib.getDeluxe(client, id) match {
            case Full(m) ⇒ Launcher.mapToObject(m)
            case e : EmptyBox ⇒
                val errmsg = (e ?~ "Empty").msg
                throw new ScriptException(errmsg)
        }
    }

    def saveDeluxe(client : Long, id : Long, data : String) : JSObject = {
        choiceletLib.saveDeluxe(client, id, data) match {
            case Full(m) ⇒ Launcher.mapToObject(m)
            case e : EmptyBox ⇒
                val errmsg = (e ?~ "Empty").msg
                throw new ScriptException(errmsg)
        }
    }
}
