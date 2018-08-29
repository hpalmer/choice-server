--
-- Created by IntelliJ IDEA.
-- User: Hep
-- Date: 6/14/2015
-- Time: 6:53 PM
--
-- Set up a server-side script on a user group.
-- Arguments:
--    group - path to the user group file
--    script - path to the script file
--    option - optional "reset", to indicate that UserScriptIndex values should be cleared
--             for all users in the group
--
local group, script, option = ...
local userlib = require "userlib"
local attrlib = require "attrlib"
local filelib = require "filelib"
local UserScriptFile = "UserScriptFile"
local UserScriptIndex = "UserScriptIndex"
-- the user should have a session. get session info.
local sinfo = userlib.getsession()
if (sinfo == nil) then
    -- that shouldn't have failed
    error("no session")
end
if (userlib.isloggedin()) then
    local ginfo, err = filelib.lookup(group)
    if (ginfo == nil) then
        error("error accessing group" .. err)
    end
    if (ginfo["mimetype"] ~= "choice/group") then
        error("not a user group: " .. group)
    end
    local rset, err = attrlib.set(group, { name=UserScriptFile; value=script })
    if (rset == nil) then
        error("error setting UserScriptFile: " .. err)
    end
    if ((#rset == 0) or (rset[1]["value"] ~= script)) then
        error("unexpected result from setting UserScriptFile")
    end
    if (option == "reset") then
        local resetUsers
        resetUsers = function(g)
            if ((g == ".") or (g == "..")) then
                return
            end
            local ulist, err = filelib.list(g, "choice/user")
            if (ulist == nil) then
                error("error listing users in " .. g .. ": ", err)
            end
            for i,uinfo in ipairs(ulist) do
                local upath = uinfo["path"]
                -- don't know if . and .. will be there, but if they are, ignore them
                local uname = uinfo["name"]
                if (uname ~= "." and uname ~= "..") then
                    local rset, err = attrlib.set(upath, { name=UserScriptIndex; value=nil })
                    if (rset == nil) then
                        error("error resetting UserScriptIndex for " .. uinfo["path"])
                    end
                end
            end
            local glist, err = filelib.list(g, "choice/group")
            if (glist == nil) then
                error("unable to list subgroups of " .. g)
            end
            for i,ginfo in ipairs(glist) do
                local gname = ginfo["name"]
                if (gname ~= "." and gname ~= "..") then
                    resetUsers(ginfo["path"])
                end
            end
        end
        resetUsers(group)
    end
    return "ok"
else
    error("not logged in")
end


