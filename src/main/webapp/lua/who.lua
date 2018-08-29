--
-- Created by IntelliJ IDEA.
-- User: Hep
-- Date: 6/15/2015
-- Time: 4:31 PM
-- To change this template use File | Settings | File Templates.
-- Helper for 'who' command
local filelib = require "filelib"
local result = {}
local activeList, err = filelib.list("/System/Sessions/active", "choice/session")
if (activeList == nil) then
    error("unable to list active sessions: " .. err)
end
for i, sinfo in ipairs(activeList) do
    local ssum, err = filelib.readsession(sinfo["path"])
    if (ssum ~= nil) then
        ssum["sessionId"] = sinfo["id"]
        result[#result + 1] = ssum
    end
end
return result
