local utils = require("utils")

local luatype = type

infoNodetypes = {
    constant = 1,
    variable = 2,
    func = 3,
    struct = 4,
    protocol = 5,
    adt = 6,
    argument = 7,
    parameter = 8,
}


literaltypes = {
    string = 1,
    number = 2,
    dictOrSet = 3,
    vector = 4
}


nodetypes = {
    letDeclaration = 1,
    varDeclaration = 2,
    funcDeclaration = 3,
    funcPrototype = 4,
    structDeclaration = 5,
    breakStmt = 6,
    conditionalStmt = 7,
    elifStmt = 8,
    elseStmt = 9,
    whileStmt = 10,
    forStmt = 11,
    reassignment = 12,
    extensionDeclaration = 13,
    protocolDeclaration = 14,
    returnStmt = 15,
    fieldDeclaration = 16,

    argument = 17,
    keywordArg = 18,
    moduleMember = 19,
    name = 20,
    literal = 21,
    expr = 22,
    slice = 23,
    subscript = 24,
    call = 25,
    field = 26,
    unaryExpr = 27,
    typeExpr = 28,
    parameterizedType = 29,
    maybe = 30,
    ref = 31,
}

local indentationLevel = 0
local indentationString = "  "


function joinBody(body)
    local length = #body
    local result = "{\n"
    indentationLevel = indentationLevel + 1
    local indStr = utils.repeatString(indentationString, indentationLevel)
    for i, stmt in ipairs(body) do
        result = result .. indStr .. stmt:toCode()
        if i < length then
            result = result .. "\n"
        end
    end
    indentationLevel = indentationLevel - 1
    return result .. utils.repeatString(indentationString, indentationLevel)
        .. "}"
end

function joinArgs(args)
    return utils.join(utils.map(Node.toCode, args))
end

function joinNames(input)
    local names = ""
    local length = #input
    for i, v in ipairs(input) do
        if v.nodetype ~= nodetypes.name then
            names = names .. "(" .. joinNames(v) .. ")"
        else
            names = names .. v:toCode()
            if i < length then
                names = names .. ", "
            end
        end
    end
    return names
end


Node = {}

function Node.new(t)
    return utils.newObject(t, Node)
end

function Node:toCode()
    if self.nodetype == nodetypes.letDeclaration then
        return string.format(
            "let %s: %s = %s",
            self.name:toCode(), self.type:toCode(), self.expr:toCode())
    elseif self.nodetype == nodetypes.varDeclaration then
        return string.format(
            "var %s: %s = %s",
            self.name:toCode(), self.type:toCode(), self.expr:toCode())
    elseif self.nodetype == nodetypes.funcDeclaration then
        return string.format(
            "fun %s(%s): %s %s\n",
            self.name:toCode(), joinArgs(self.args), self.type:toCode(),
            joinBody(self.body))
    elseif self.nodetype == nodetypes.funcPrototype then
        return string.format(
            "fun %s(%s): %s",
            self.name:toCode(), joinArgs(self.args), self.type:toCode())
    elseif self.nodetype == nodetypes.structDeclaration then
        return string.format(
            "struct %s(%s) is (%s) %s\n",
            self.name:toCode(), joinArgs(self.parameters),
            joinArgs(self.protocols), joinBody(self.body))
    elseif self.nodetype == nodetypes.breakStmt then
        return "break"
    elseif self.nodetype == nodetypes.conditionalStmt then
        local elseStmt = ""
        if self.elseStmt then
            elseStmt = self.elseStmt:toCode()
        end
        return string.format(
            "if %s %s%s %s",
            self.expr:toCode(), joinBody(self.body),
            utils.join(utils.map(Node.toCode, self.elifs), " "), elseStmt)
    elseif self.nodetype == nodetypes.elifStmt then
        return string.format(
            "elif %s %s", self.expr:toCode(), joinBody(self.body))
    elseif self.nodetype == nodetypes.elseStmt then
        return string.format("else %s", joinBody(self.body))
    elseif self.nodetype == nodetypes.whileStmt then
        return string.format(
            "while %s %s", self.expr:toCode(), joinBody(self.body))
    elseif self.nodetype == nodetypes.forStmt then
        return string.format(
            "for (%s) in %s %s",
            joinNames(self.names), self.container:toCode(), joinBody(self.body))
    elseif self.nodetype == nodetypes.reassignment then
        return string.format(
            "%s %s %s", self.left:toCode(), self.op, self.right:toCode())
    elseif self.nodetype == nodetypes.extensionDeclaration then
        return string.format(
            "extension %s(%s) is (%s) %s\n",
            self.name:toCode(), joinArgs(self.parameters),
            joinArgs(self.protocols), joinBody(self.body))
    elseif self.nodetype == nodetypes.protocolDeclaration then
        return string.format(
            "protocol %s(%s) is (%s) %s\n",
            self.name:toCode(), joinArgs(self.parameters),
            joinArgs(self.protocols), joinBody(self.body))
    elseif self.nodetype == nodetypes.returnStmt then
        return string.format("return %s", self.expr:toCode())
    elseif self.nodetype == nodetypes.fieldDeclaration then
        return string.format("%s: %s", self.name:toCode(), self.type:toCode())
    elseif self.nodetype == nodetypes.argument then
        return string.format(
            "%s: %s = %s",
            utils.join(utils.map(Node.toCode, self.names), "", " "),
            self.type:toCode(), self.expr)
    elseif self.nodetype == nodetypes.keywordArg then
        return string.format("%s = %s", self.name:toCode(), self.expr:toCode())
    elseif self.nodetype == nodetypes.moduleMember then
        return string.format(
            "%s#%s", self.module:toCode(), self.member:toCode())
    elseif self.nodetype == nodetypes.name then
        return self.string
    elseif self.nodetype == nodetypes.literal then
        return self.text
    elseif self.nodetype == nodetypes.expr then
        return string.format(
            "%s %s %s", self.left:toCode(), self.op, self.right:toCode())
    elseif self.nodetype == nodetypes.slice then
        local op = "..<"
        if self.isInclusive then
            op = "..."
        end
        local stepString = ""
        if self.step then
            stepString = ":" .. self.step:toCode()
        end
        return string.format(
            "%s[%s%s%s%s]",
            self.base:toCode(), self.first:toCode(), op, self.last:toCode(),
            stepString)
    elseif self.nodetype == nodetypes.subscript then
        return string.format("%s[%s]", self.base:toCode(), self.key:toCode())
    elseif self.nodetype == nodetypes.call then
        return string.format("%s(%s)", self.base:toCode(), joinArgs(self.args))
    elseif self.nodetype == nodetypes.field then
        return string.format("%s.%s", self.base:toCode(), self.field:toCode())
    elseif self.nodetype == nodetypes.unaryExpr then
        return self.op .. " " .. self.expr:toCode()
    elseif self.nodetype == nodetypes.typeExpr then
        if self.op == "is" then
            return string.format(
                "%s is (%s)", self.left:toCode(), joinArgs(self.right))
        end
        return string.format(
            "%s %s %s", self.left:toCode(), self.op, self.right:toCode())
    elseif self.nodetype == nodetypes.parameterizedType then
        return string.format(
            "%s(%s)", self.base:toCode(), joinArgs(self.parameters))
    elseif self.nodetype == nodetypes.maybe then
        return self.base:toCode() .. "?"
    elseif self.nodetype == nodetypes.ref then
        return string.format("ref(%s)", self.base:toCode())
    end
end

local tableIndentationLevel = 0
local tableIndentationString = "    "

function tableToString(table)
    local result = "{\n"
    local length = #table
    tableIndentationLevel = tableIndentationLevel + 1
    local indStr = utils.repeatString(
        tableIndentationString, tableIndentationLevel)
    local i = 1
    for k, v in pairs(table) do
        local string
        if luatype(v) == "table" then
            string = tableToString(v)
        else
            string = tostring(v)
        end
        result = result .. indStr .. tostring(k) .. " = " .. string .. ",\n"
        i = i + 1
    end
    tableIndentationLevel = tableIndentationLevel - 1
    return result ..
        utils.repeatString(tableIndentationString, tableIndentationLevel) .. "}"
end

function Node:__tostring()
    return tableToString(self)
end


return _G
