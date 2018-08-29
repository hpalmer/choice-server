/**
  * Copyright © 2014-2016 The Board of Trustees of The Leland Stanford Junior University.
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
  * Lua BaseLib extension for Cfs.
  *
  * @author Howard Palmer
  * Created by Hep on 5/20/2014.
  */
package choice.script.lua

import java.io.{IOException, InputStream}

import choice.access.Principal
import choice.fs.CfsFiles
import net.liftweb.common.Logger
import org.luaj.vm2.LuaValue
import org.luaj.vm2.lib.BaseLib

class LuaBaseLib(chenv : ChoiceEnviron) extends BaseLib {
    import LuaBaseLib.Log

    def getPrincipal : Principal = chenv.principal

    val iolib : FsIoLib = chenv.iolib

    /** Extend the library loading to set the default value for Globals.STDIN */
    override def call(modname: LuaValue, env: LuaValue): LuaValue = {
        super.call(modname, env)
    }

    /**
     * Try to open a file in the current working directory,
     * or fall back to base opener if not found.
     *
     * This implementation attempts to open the file using new File(filename).
     * It falls back to the base implementation that looks it up as a resource
     * in the class path if not found as a plain file.
     *
     * @see org.luaj.vm2.lib.BaseLib
     * @see org.luaj.vm2.lib.ResourceFinder
     *
     * @param filename the file name
     * @return InputStream, or null if not found.
     */
    override def findResource(filename : String) : InputStream = {
        implicit val principalImpl : () ⇒ Principal = chenv.principal _
        try {
            chenv.cfsFileLib.getAbsolutePath(filename) map { path ⇒
                CfsFiles.newInputStream(path)
            } openOr null
        }
        catch {
            case iox : IOException ⇒
                Log.error(s"resource error on $filename", iox)
                null
        }
    }
}

object LuaBaseLib {
    val Log = Logger("choice.script.lua.LuaBaseLib")
}
