local utils = require("utils")
local astlib = require("astlib")


Context = {}

function Context.new(modulePaths, mainFileHash, loadedModules)
    return utils.newObject(
        {
            env = Env.new(),
            modulePaths = modulePaths,
            mainFileHash = mainFileHash or "",
            loadedModules = loadedModules or {},
            loadedLines = {},
            tmpCounter = 0,
            parentStruct = nil,
        },
        Context)
end

function Context:copy()
    return utils.newObject(
        {
            env = Env.new(),
            modulePaths = self.modulePaths,
            mainFileHash = self.mainFileHash,
            loadedModules = self.loadedModules,
            loadedLines = {},
            tmpCounter = self.tmpCounter,
            parentStruct = nil,
        },
        Context)
end


Env = {}

function Env.new()
    return utils.newObject({scope = 1, space = {[1] = {}}}, Env)
end

function Env:addScope()
    self.scope = self.scope + 1
    self.space[self.scope] = {}
end

function Env:removeScope()
    table.remove(self.space, self.scope)
    self.scope = self.scope - 1
end

function Env:__newindex(key, node)
    self.space[self.scope][key] = node
end

function Env:contains(key)
    local scope = self.scope
    while scope > 0 do
        local found = self.space[scope][key]
        if found then
            return found
        end
        scope = scope - 1
    end
    return nil
end

function Env:get(key)
    local result = self:contains(key)
    if result then
        return result
    else
        error(
            string.format("ADRIAN INTERNAL ERROR: unknown name '%s'", key), 2)
    end
end

return _G
