--
-- Created by IntelliJ IDEA.
-- User: Hep
-- Date: 2/17/2017
-- Time: 12:06 AM
-- To change this template use File | Settings | File Templates.
--
local option, optarg = ...
local userlib = require "userlib"
local attrlib = require "attrlib"
local GroupDataFile = "GroupDataFile"
local GroupDataIndex = "GroupDataIndex"
-- the user should have a session. get session info.
local sinfo = userlib.getsession()
if (sinfo == nil) then
    -- that shouldn't have failed
    error("no session")
end
-- get the path to the user's current group
local group = sinfo.logingroup
-- the logingroup may not be the current group
for _,g in ipairs(sinfo["groups"]) do
    if (g["gid"] == sinfo["cgid"]) then
        group = g["paths"][1]
        break
    end
end

-- the group may have an attribute referencing a text file, and an index (line number) for that file
local attr = attrlib.get(group, GroupDataFile, GroupDataIndex)
if (attr ~= nil and #attr > 0 and attr[1]["value"] ~= nil) then
    -- get the path to the data file for the group
    local datafile = attr[1]["value"]
    -- if no index value, initialize the data file index to zero
    if ((#attr == 1) or (attr[2]["value"] == nil)) then
        local rset, err = attrlib.set(group, { name=GroupDataIndex, value=0 })
        if (rset == nil) then
            error(err)
        end
        if (#rset == 0) then
            error("failed to set GroupDataIndex")
        end
        if ((rset[1]["name"] ~= GroupDataIndex) or (rset[1]["value"] ~= 0)) then
            error("unexpected result from setting GroupDataIndex")
        end
        attr = attrlib.get(group, GroupDataIndex)
    else
        attr= { attr[2] }
    end
    -- now there should be a data file index value
    if (attr ~= nil and #attr > 0 and attr[1]["value"] ~= nil) then
        local dindex = attr[1]["value"]
        local newIndex = dindex + 1
        -- is there an optional action?
        if (option ~= nil) then
            if (option == "reset") then
                dindex = 0
                newIndex = 0
            elseif (option == "page") then
                if (type(optarg) == "number") then
                    dindex = optarg
                else
                    dindex = 0
                end
                newIndex = dindex
            end
        end

        -- update the GroupDataIndex value
        local rset, err = attrlib.set(group, { name=GroupDataIndex, value= newIndex })
        if ((rset == nil) or (#rset == 0) or (rset[1]["value"] ~= newIndex)) then
            error(err or "unexpected result from setting GroupDataIndex")
        end

        -- read the lines of the GroupDataFile
        local io = require "io"
        local lines = {}
        for line in io.lines(datafile) do
            table.insert (lines, line)
        end
        -- select the line to return
        local lineno = (dindex % #lines) + 1
        return lines[lineno]
    end
else
    -- signal error
    error("no GroupDataFile value for group")
end
