function is(t, metatable)
    local mt = getmetatable(t)
    if mt then
        return mt["__index"] == metatable
    end
    return false
end

function isAny(t, metatables)
    for _, v in ipairs(metatables) do
        if is(t, v) then
            return true
        end
    end
    return false
end

function map(func, t)
    local result = {}
    for _, v in ipairs(t) do
        table.insert(result, func(v))
    end
    return result
end

function join(table, before, after)
    local result = ""
    local before = before or ""
    local after = after or ", "
    local length = #table
    for i, v in ipairs(table) do
        result = result .. before .. tostring(v)
        if i < length then
            result = result .. after
        end
    end
    return result
end

function newObject(obj, baseClass)
    local mt = {
        __index = baseClass,
        __newindex = baseClass.__newindex,
        __tostring = baseClass.__tostring
    }
    return setmetatable(obj, mt)
end

function adrianError(message, possibleFix)
    print(string.format("ERROR: %s.", message))
    if possibleFix then
        print(string.format("  FIX: %s.", possibleFix))
    end
    os.exit()
end

function notImplemented()
    print("NOT IMPLEMENTED")
    os.exit()
end

function repeatString(string, times)
    local result = ""
    for i = times, 1, -1 do
        result = result .. string
    end
    return result
end

adrianDebug = {
    printTable = function (tab, level)
        if not level then
            print("printTable start")
        end
        local lvl = (level or 0) + 1
        for k, v in pairs(tab) do
            print(repeatString("* ", lvl), k, v)
            if type(v) == "table" then
                adrianDebug.printTable(v, lvl)
            end
        end
        if not level then
            print("printTable end")
        end
    end
}

return _G
