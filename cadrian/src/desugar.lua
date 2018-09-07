local astlib = require("astlib")
local context = require("context")
local utils = require("utils")
local is = utils.is

local UNKNOWN_NAME = "unknown name '%s'"
local UNKNOWN_NAME_FIX = "probably you need to declare that name"

local builtinFuncs = {}
local luatype = type


function checkNameExistence(name, context)
    if context.env:contains(name.string) then
        return name
    end
    utils.adrianError(
        string.format(UNKNOWN_NAME, name.string), UNKNOWN_NAME_FIX)
end


function t(type, context)
    if type == nil then
        return nil
    elseif type.nodetype == astlib.nodetypes.name then
        return checkNameExistence(type, context)
    elseif type.nodetypes == astlib.nodetypes.moduleMember then
        return type
    elseif type.nodetypes == astlib.nodetypes.ref then
        return astlib.Node.new({
            nodetype = type.nodetypes,
            base = t(type.base, context)
        })
    else
        for i,v in ipairs(getmetatable(type)) do
            print(i,v)
        end
        error("not implemented t:desugar")
    end
end


function a(args, context)
    local result = {}
    for _, v in ipairs(args) do
        table.insert(result, e(v, context))
    end
    return result
end


function e(expr, context)
    if expr.nodetype == astlib.nodetypes.call then
        if expr.base.nodetype == astlib.nodetypes.name and
                builtinFuncs[expr.base.string] then
            utils.notImplemented()
        end
        return astlib.Node.new({
            nodetype = astlib.nodetypes.call,
            base = e(expr.base, context),
            args = a(expr.args, context)
        })
    elseif expr.nodetype == astlib.nodetypes.subscript then
        return astlib.Node.new({
            nodetype = astlib.nodetypes.subscript,
            base = e(expr.base, context),
            key = e(expr.key, context)
        })
    elseif expr.nodetype == astlib.nodetypes.field then
        return astlib.Node.new({
            nodetype = astlib.nodetypes.field,
            base = e(expr.base, context),
            field = expr.field
        })
    elseif expr.nodetype == astlib.nodetypes.slice then
        return astlib.Node.new({
            nodetype = astlib.nodetypes.slice,
            base = e(expr.base, context),
            first = e(expr.first, context),
            isInclusive = expr.isInclusive,
            last = e(expr.last, context),
            step = e(expr.step, context)
        })
    elseif expr.nodetype == astlib.nodetypes.maybe then
        return astlib.Node.new({
            nodetype = astlib.nodetypes.maybe,
            base = e(expr.base, context)
        })
    elseif expr.nodetype == astlib.nodetypes.ref then
        return astlib.Node.new({
            nodetype = astlib.nodetypes.ref,
            base = e(expr.base, context)
        })
    elseif expr.nodetype == astlib.nodetypes.name then
        return checkNameExistence(expr, context)
    elseif expr.nodetype == astlib.nodetypes.moduleMember then
        return expr
    elseif expr.nodetype == astlib.nodetypes.literal then
        return expr
    elseif expr == nil then
        return nil
    else
        error("not implemented e:desugar")
    end
end


function astlib.Node:desugar(context)
    if self.nodetype == astlib.nodetypes.letDeclaration then
        local node = astlib.Node.new({
            nodetype = self.nodetype,
            name = self.name,
            type = t(self.type, context),
            expr = e(self.expr, context)
        })
        node:register(context)
        coroutine.yield(node)
    end
end


function astlib.Node:register(context)
    context.env[self.name.string] = {
        infoNodetype = astlib.infoNodetype.constant,
        type = self.type,
        expr = self.expr
    }
end


function main(ast, context)
    local result = {}
    for _, node in ipairs(ast) do
        local co = coroutine.create(node.desugar)
        local status, newNode = coroutine.resume(co, node, context)
        if status == false then
            error(newNode)
        end
        while status == true do
            table.insert(result, newNode)
            status, newNode = coroutine.resume(co, node, context)
        end
    end
    return result
end


return {main = main}
