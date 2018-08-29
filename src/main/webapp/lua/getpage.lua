local option, optarg = ...
local userlib = require "userlib"
local attrlib = require "attrlib"
local UserScriptFile = "UserScriptFile"
local UserScriptIndex = "UserScriptIndex"
-- the user should have a session. get session info.
local sinfo = userlib.getsession()
if (sinfo == nil) then
    -- that shouldn't have failed
    error("no session")
end
-- get the path to the user's current group
local group = sinfo.logingroup
-- the logingroup may not be the current group
for i,g in ipairs(sinfo["groups"]) do
    if (g["gid"] == sinfo["cgid"]) then
        group = g["paths"][1]
        break
    end
end

if (userlib.isloggedin()) then
    -- the group may have an attribute referencing a script file
    local attr = attrlib.get(group, UserScriptFile)
    if (attr ~= nil and #attr > 0 and attr[1]["value"] ~= nil) then
        -- get the path to the script file for the group
        local scriptfile = attr[1]["value"]
        -- see if the current user has a script index value
        attr = attrlib.get(sinfo.user.path, UserScriptIndex)
        -- if not, initialize the user's script index to zero
        if ((attr == nil) or (#attr == 0) or (attr[1]["value"] == nil)) then
            local rset, err = attrlib.set(sinfo.user.path, { name=UserScriptIndex, value=0 })
            if (rset == nil) then
                error(err)
            end
            if (#rset == 0) then
                error("failed to set UserScriptIndex")
            end
            if ((rset[1]["name"] ~= UserScriptIndex) or (rset[1]["value"] ~= 0)) then
                error("unexpected result from setting UserScriptIndex")
            end
            attr = attrlib.get(sinfo.user.path, UserScriptIndex)
        end
        -- now the user should have a script index value
        if (attr ~= nil and #attr > 0 and attr[1]["value"] ~= nil) then
            local sindex = attr[1]["value"]
            -- is there an optional action?
            if (option ~= nil) then
                local doset = false
                if (option == "next") then
                    sindex = sindex + 1
                    doset = true
                elseif (option == "reset") then
                    sindex = 0
                    doset = true
                elseif (option == "page") then
                    doset = true
                    if (type(optarg) == "number") then
                        sindex = optarg
                    else
                        sindex = 0
                    end
                end
                if (doset) then
                    local rset, err = attrlib.set(sinfo.user.path, { name=UserScriptIndex, value=sindex })
                    if ((rset == nil) or (#rset == 0) or (rset[1]["value"] ~= sindex)) then
                        error(err or "unexpected result from setting UserScriptIndex")
                    end
                end
            end

            local io = require "io"
            local findit = function()
                local n = 0
                for line in io.lines(scriptfile) do
                    if (n == sindex) then
                        return line
                    end
                    n = n + 1
                end
                -- fell off the end of the script, so punt
                return "logout.html"
            end
            -- look for the indicated line in the script file
            local ok, result = pcall(findit)
            if (ok) then
                -- return it if found
                return result
            end
            error("cannot access script file")
        end
    else
        -- signal error so nextPage can fall back to client-side scripting
        error("no UserScriptFile value for group")
--        local filelib = require "filelib"
--        local ginfo = filelib.lookup(group)
--        if (ginfo.home ~= nil) then
--            return ginfo.home
--        end
--        -- put no script and no home page for the group, so punt
--        return "home.html"
    end
else
    -- user is not logged in, try for the group's guest page
    local filelib = require "filelib"
    local ginfo = filelib.lookup(group)
    if (ginfo.guest ~= nil) then
        return ginfo.guest
    end
    -- no guest page, so punt
    return "index.html"
end
