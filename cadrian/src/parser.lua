local lpeg = require("lpeg")
local astlib = require("astlib")
local utils = require("utils")

local luatype = type


local letter = lpeg.R("az") + lpeg.R("AZ")
local number = lpeg.R("09")
local underscore = lpeg.P("_")
local pw = lpeg.S(" \n")^0
local keywords =
    lpeg.P("let ") + lpeg.P("var ") + lpeg.P("fun ") + lpeg.P("protocol ") +
    lpeg.P("struct ") + lpeg.P("adt ") + lpeg.P("not ") + lpeg.P("in ") +
    lpeg.P("is ") + lpeg.P("for ") + lpeg.P("while ") + lpeg.P("return ") +
    lpeg.P("if ") + lpeg.P("elif ") + lpeg.P("else ") + lpeg.P("and ") +
    lpeg.P("or ")

local numberLiteral = number^1
local stringLiteral = lpeg.P('"') * (lpeg.P(1) - '"')^0 * lpeg.P('"')


local function parameterizedTypeInit(p)
    return p / function (base, parameters)
        if parameters == nil then
            return base
        end
        return astlib.Node.new({
            nodetype = astlib.nodetypes.parameterizedType,
            base = base,
            parameters = parameters
        })
    end
end

local function typeCombInit(p)
    return p / function (left, op, right)
        if op == nil then
            return left
        end
        return astlib.Node.new({
            nodetype = astlib.nodetypes.typeExpr,
            left = left,
            op = op,
            right = right
        })
    end
end

local function refInit(p)
    return p / function (base)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.ref,
            base = base
        })
    end
end

local function maybeInit(p)
    return p / function (base, mark)
        if mark == nil then
            return base
        end
        return astlib.Node.new({
            nodetype = astlib.nodetypes.maybe,
            base = base
        })
    end
end

local function typeDependencyInit(p)
    return p / function (base, protocols)
        if protocols == nil then
            return base
        end
        return astlib.Node.new({
            nodetype = astlib.nodetypes.typeExpr,
            left = base,
            op = "is",
            right = protocols
        })
    end
end


local function nameInit(p)
    return p / function (string)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.name,
            string = string
        })
    end
end

local function vectorLiteralInit(p)
    return p / function (text)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.literal,
            type = astlib.literaltypes.vector,
            text = text
        })
    end
end

local function dictOrSetLiteralInit(p)
    return p / function (text)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.literal,
            type = astlib.literaltypes.dictOrSet,
            text = text
        })
    end
end

local function numberLiteralInit(p)
    return p / function (text)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.literal,
            type = astlib.literaltypes.number,
            text = text
        })
    end
end

local function stringLiteralInit(p)
    return p / function (text)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.literal,
            type = astlib.literaltypes.string,
            text = text
        })
    end
end

local function letDeclInit(p)
    return p / function (name, type, expr)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.letDeclaration,
            name = name,
            type = type,
            expr = expr
        })
    end
end

local function varDeclInit(p)
    return p / function (name, type, expr)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.varDeclaration,
            name = name,
            type = type,
            expr = expr
        })
    end
end

local function funcProtoInit(p)
    return p / function (name, args, type)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.funcPrototype,
            name = name,
            args = args,
            type = type
        })
    end
end

local function funcDeclInit(p)
    return p / function (funcProto, body)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.funcDeclaration,
            name = funcProto.name,
            args = funcProto.args,
            type = funcProto.type,
            body = body
        })
    end
end

local function argumentInit(p)
    return p / function (name, type, expr)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.argument,
            name = name,
            type = type,
            expr = expr
        })
    end
end

local function protoDeclInit(p)
    return p / function (name, parameters, protocols, body)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.protocolDeclaration,
            name = name,
            parameters = parameters,
            protocols = protocols,
            body = body
        })
    end
end

local function structDeclInit(p)
    return p / function (name, parameters, protocols, body)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.structDeclaration,
            name = name,
            parameters = parameters,
            protocols = protocols,
            body = body
        })
    end
end

local function extDeclInit(p)
    return p / function (name, parameters, protocols, body)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.extensionDeclaration,
            name = name,
            parameters = parameters,
            protocols = protocols,
            body = body
        })
    end
end

local function reassignmentInit(p)
    return p / function (left, op, right)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.reassignment,
            left = left,
            op = op,
            right = right
        })
    end
end

local function forStmtInit(p)
    return p / function (names, container, body)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.forStmt,
            names = names,
            container = container,
            body = body
        })
    end
end

local function whileStmtInit(p)
    return p / function (expr, body)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.whileStmt,
            expr = expr,
            body = body
        })
    end
end

local function condStmtInit(p)
    return p / function (expr, body, elifs, elseStmt)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.elseStmt,
            expr = expr,
            body = body,
            elifs = elifs,
            elseStmt = elseStmt
        })
    end
end

local function elifStmtInit(p)
    return p / function (expr, body)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.elifStmt,
            expr = expr,
            body = body
        })
    end
end

local function elseStmtInit(p)
    return p / function (body)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.elseStmt,
            body = body
        })
    end
end

local function breakStmtInit(p)
    return p / function (input)
        return astlib.Node.new({nodetype = astlib.nodetypes.breakStmt})
    end
end

local function returnInit(p)
    return p / function (expr)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.returnStmt,
            expr = expr
        })
    end
end

local function fieldDeclInit(p)
    return p / function (name, type)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.FieldDeclaration,
            name = name,
            type = type
        })
    end
end

local function moduleMemberOrKeywordArg(p)
    return p / function (name, trailer)
        if trailer == nil then
            return name
        elseif trailer.nodetype == "keywordT" then
            return astlib.Node.new({
                nodetype = astlib.nodetypes.keywordArg,
                name = name,
                expr = trailer.expr,
            })
        else
            return astlib.Node.new({
                nodetype = astlib.nodetypes.moduleMember,
                module = name,
                member = trailer.member,
            })
        end
    end
end

local function addTables(acc, value)
    for _, v in ipairs(value) do
        table.insert(acc, v)
    end
    return acc
end

local function opt(p)
    return p / function (someValue)
        return someValue
    end
end

local function optNil(p)
    return p / function (someValue)
        if (luatype(someValue) ~= "table") or
                (someValue.nodetype == nil and #someValue == 0) then
            return nil
        end
        return someValue
    end
end

local function optStr(p, string)
    return p / function (someValue)
        if string == someValue then
            return string
        end
        return nil
    end
end


local function functionTrailer(p)
    return p / function (argList)
        return {nodetype = "callT", args = argList}
    end
end

local function structFieldTrailer(p)
    return p / function (fieldName)
        return {nodetype = "fieldT", field = fieldName}
    end
end


local function atomExprInit(p)
    return p / function (atom, gotTrailers)
        local trailers = {}
        for _, v in ipairs(gotTrailers) do
            if v ~= nil then
                table.insert(trailers, v)
            end
        end
        local result = atom
        for _, v in ipairs(trailers) do
            if v.nodetype == "callT" then
                result = astlib.Node.new({
                    nodetype = astlib.nodetypes.call,
                    base = result,
                    args = v.args
                })
            elseif v.nodetype == "subscriptT" then
                result = astlib.Node.new({
                    nodetype = astlib.nodetypes.subscript,
                    base = result,
                    key = v.value
                })
            elseif v.nodetype == "sliceT" then
                result = astlib.Node.new({
                    nodetype = astlib.nodetypes.slice,
                    base = result,
                    first = v.first,
                    isInclusive = v.isInclusive,
                    last = v.last,
                    step = v.step
                })
            elseif v.nodetype == "unwrapT" then
                result = astlib.Node.new({
                    nodetype = astlib.nodetypes.maybe,
                    base = result
                })
            elseif v.nodetype == "fieldT" then
                result = astlib.Node.new({
                    nodetype = astlib.nodetypes.field,
                    base = result,
                    field = v.field
                })
            end
        end
        return result
    end
end

local function sliceOrSubscriptInit(p)
    return p / function (left, rangeOp, right, step)
        if rangeOp == nil then
            return {nodetype = "subscriptT", key = left}
        end
        return {
            nodetype = "sliceT", first = left, isInclusive = (rangeOp == "..."),
            last = right, step = step
        }
    end
end

local function unwrapTrailer(p)
    return p / function (mark)
        if mark == "?" then
            return {nodetype = "unwrapT"}
        end
        return nil
    end
end


function table.slice(t, first, last)
    local result = {}
    local j = 1
    for i = first, last or #t do
        result[j] = t[i]
        j = j + 1
    end
    return result
end


local function _makeExpr(left, other)
    if #other == 0 then
        return left
    elseif #other == 2 then
        return astlib.Node.new({
            nodetype = astlib.nodetypes.expr,
            left = left,
            op = other[1],
            right = other[2]
        })
    else
        return astlib.Node.new({
            nodetype = astlib.nodetypes.expr,
            left = left,
            op = other[1],
            right = _makeExpr(other[2], table.slice(other, 3))
        })
    end
end

local function makeExpr(p)
    return p / _makeExpr
end

local function unaryOperator(p)
    return p / function (op, expr)
        return astlib.Node.new({
            nodetype = astlib.nodetypes.unaryExpr,
            op = op,
            expr = expr
        })
    end
end

local function moduleMemberTrailerInit(p)
    return p / function (member)
        return {nodetype = "moduleMemberT", member = member}
    end
end

local function keywordArgTrailerInit(p)
    return p / function (expr)
        return {nodetype = "keywordT", expr = expr}
    end
end


local grammar = lpeg.P({
    "ast",
    ast = lpeg.Ct(lpeg.V("statement")^1) + lpeg.Ct(lpeg.P("")),
    statement = pw * (
        lpeg.V("letDecl") + lpeg.V("varDecl") + lpeg.V("reassignment") +
        lpeg.V("forStmt") + lpeg.V("whileStmt") + lpeg.V("condStmt") +
        lpeg.V("funcDecl") + lpeg.V("protoDecl") + lpeg.V("structDecl") +
        lpeg.V("extensionDecl")
    ) * pw,


    letDecl = letDeclInit(
        lpeg.P("let") * pw * lpeg.V("name") * pw *
        optNil((lpeg.P(":") * pw * lpeg.V("type") * pw)^-1) *
        lpeg.P("=") * pw * lpeg.V("expr")),
    varDecl = varDeclInit(
        lpeg.P("var") * pw * lpeg.V("name") * pw *
        optNil((lpeg.P(":") * pw * lpeg.V("type") * pw)^-1) *
        lpeg.P("=") * pw * lpeg.V("expr")),
    reassignment = reassignmentInit(
        lpeg.V("expr") * pw * lpeg.V("reassignmentOp") * pw * lpeg.V("expr")),
    funcProto = funcProtoInit(
        lpeg.P("fun") * pw * lpeg.V("name") * lpeg.P("(") * opt(lpeg.V("args")) *
        lpeg.P("):") * pw * lpeg.V("type")),
    funcDecl = funcDeclInit(
        lpeg.V("funcProto") * pw * lpeg.P("{") * lpeg.Ct(lpeg.V("funcStmt")^0) *
        lpeg.P("}")),
    protoDecl = protoDeclInit(
        lpeg.P("protocol") * pw * lpeg.V("name") * opt(lpeg.V("parameters")) *
        pw * opt(lpeg.V("protocols")) * lpeg.P("{") *
        lpeg.Ct(lpeg.V("protocolStmt")^0) * lpeg.P("}")),
    structDecl = structDeclInit(
        lpeg.P("struct") * pw * lpeg.V("name") * opt(lpeg.V("parameters")) *
        pw * opt(lpeg.V("protocols")) * lpeg.P("{") *
        lpeg.Ct(lpeg.V("structStmt")^0) * lpeg.P("}")),
    extensionDecl = extDeclInit(
        lpeg.P("extension") * pw * lpeg.V("name") * opt(lpeg.V("parameters")) *
        pw * opt(lpeg.V("protocols")) * lpeg.P("{") *
        lpeg.Ct(lpeg.V("structStmt")^0) * lpeg.P("}")),
    returnStmt = returnInit(lpeg.P("return") * pw * lpeg.V("expr")),
    fieldDecl = fieldDeclInit(
        lpeg.V("name") * lpeg.P(":") * pw * lpeg.V("type")),
    forStmt = forStmtInit(
        lpeg.P("for") * pw * lpeg.V("forElemNames") * pw *
        lpeg.P("in") * pw * lpeg.V("expr") * pw * lpeg.P("{") *
        lpeg.Ct((lpeg.V("breakStmt") + lpeg.V("statement"))^0) * lpeg.P("}")),
    whileStmt = whileStmtInit(
        lpeg.P("while") * pw * lpeg.V("condExpr") * pw * lpeg.P("{") *
        lpeg.Ct((lpeg.V("breakStmt") + lpeg.V("statement"))^0) * lpeg.P("}")),
    condStmt = condStmtInit(
        lpeg.P("if") * pw * lpeg.V("condExpr") * pw * lpeg.P("{") * pw *
        lpeg.Ct(lpeg.V("statement")^0) * lpeg.P("}") * pw *
        lpeg.Ct(lpeg.V("elifStmt")^0) * pw * opt(lpeg.V("elseStmt"))),
    elifStmt = elifStmtInit(
        lpeg.P("elif") * pw * lpeg.V("condExpr") * pw * lpeg.P("{") *
        lpeg.Ct(lpeg.V("statement")^0) * lpeg.P("}") * pw),
    elseStmt = elseStmtInit(
        lpeg.P("else") * pw * lpeg.P("{") *
        lpeg.Ct(lpeg.V("statement")^0) * lpeg.P("}") + lpeg.P("")),
    breakStmt = breakStmtInit(pw * lpeg.P("break") * pw),


    funcStmt = pw * (
        lpeg.V("letDecl") + lpeg.V("varDecl") + lpeg.V("reassignment") +
        lpeg.V("forStmt") + lpeg.V("returnStmt") + lpeg.V("whileStmt") +
        lpeg.V("condStmt") + lpeg.V("funcDecl")
    ) * pw,
    protocolStmt = pw * (lpeg.V("funcProto") + lpeg.V("fieldDecl")) * pw,
    structStmt = pw * (lpeg.V("funcDecl") + lpeg.V("fieldDecl")) * pw,

    keywordArgTrailer =
        keywordArgTrailerInit(
            pw * lpeg.P("=") * pw * lpeg.V("expr")),
    reassignmentOp =
        lpeg.Cs(
            lpeg.P("+=") + lpeg.P("-=") + lpeg.P("*=") + lpeg.P("/=") +
            lpeg.P("=")),
    condExpr = lpeg.V("letDecl") + lpeg.V("expr"),
    forElemNames =
        lpeg.Ct(lpeg.V("name")) +
        lpeg.P("(") * lpeg.V("namesOrForElemNames") * lpeg.P(")"),
    namesOrForElemNames =
        lpeg.Cf(
            lpeg.Ct(
                lpeg.V("name") + lpeg.V("forElemNames")) *
                (lpeg.P(",") * pw * lpeg.V("namesOrForElemNames"))^0,
            addTables) +
        lpeg.P(""),
    protocols = (
        lpeg.P("is") * pw * lpeg.P("(") * lpeg.V("types") * lpeg.P(")") * pw +
        lpeg.P("")),
    types =
        lpeg.Cf(
            lpeg.Ct(lpeg.V("type")) * (lpeg.P(",") * pw * lpeg.V("types"))^0,
            addTables) +
        lpeg.P(""),
    parameters = (lpeg.P("(") * lpeg.V("names") * lpeg.P(")") + lpeg.P("")),
    names =
        lpeg.Cf(
            lpeg.Ct(lpeg.V("name")) * (lpeg.P(",") * pw * lpeg.V("names"))^0,
            addTables) +
        lpeg.P(""),
    namesSepBySpace =
        lpeg.Cf(
            lpeg.Ct(lpeg.V("name")) * (lpeg.P(" ") * lpeg.V("namesSepBySpace"))^0,
            addTables) +
        lpeg.P(""),
    args =
        lpeg.Cf(
            lpeg.Ct(lpeg.V("argument")) * (lpeg.P(",") * pw * lpeg.V("args"))^0,
            addTables) +
        lpeg.P(""),
    argument = argumentInit(
        lpeg.V("namesSepBySpace") * lpeg.P(":") * pw * lpeg.V("type") *
        pw * optNil((lpeg.P("=") * pw * lpeg.V("expr"))^-1)),
    name =
        nameInit(
            lpeg.Cs(
                (letter + underscore) *
                (letter + underscore + number)^0)) - keywords,
    moduleMemberTrailer = moduleMemberTrailerInit(lpeg.P("#") * lpeg.V("name")),
    argList =
        lpeg.Cf(
            lpeg.Ct(lpeg.V("expr")) * (lpeg.P(",") * pw * lpeg.V("argList"))^0,
            addTables) +
        lpeg.P(""),
    vectorLiteral = vectorLiteralInit(
        lpeg.P("[") * pw * lpeg.V("argList") * pw * lpeg.P("]")),
    dictOrSetLiteral = dictOrSetLiteralInit(
        lpeg.P("{") * pw * lpeg.V("argList") * pw * lpeg.P("}")),

    expr = lpeg.V("orTest"),
    orTest =
        makeExpr(
            lpeg.V("andTest") *
            lpeg.Ct((pw * lpeg.Cs(lpeg.P("or")) * pw * lpeg.V("andTest"))^0)),
    andTest =
        makeExpr(
            lpeg.V("notTest") *
            lpeg.Ct((pw * lpeg.Cs(lpeg.P("and")) * pw * lpeg.V("notTest"))^0)),
    notTest =
        unaryOperator(lpeg.Cs(lpeg.P("not")) * pw * lpeg.V("notTest")) +
        lpeg.V("comparison"),
    comparison =
        makeExpr(
            lpeg.V("subExpr") *
            lpeg.Ct((pw * lpeg.Cs(lpeg.V("comparisonOp")) * pw * lpeg.V("subExpr"))^0)),
    subExpr = lpeg.V("arithExpr"),
    arithExpr =
        makeExpr(
            lpeg.V("term") *
            lpeg.Ct((pw * lpeg.Cs(lpeg.V("addSubOp")) * pw * lpeg.V("term"))^0)),
    term = makeExpr(
        (lpeg.V("factor") *
            lpeg.Ct((pw * lpeg.Cs(lpeg.V("mulDivOp")) * pw * lpeg.V("factor"))^0))),
    factor =
        unaryOperator(lpeg.Cs(lpeg.V("addSubOp")) * pw * lpeg.V("factor")) +
        lpeg.V("power"),
    power = makeExpr(
        lpeg.V("atomExpr") *
        lpeg.Ct((pw * lpeg.Cs(lpeg.P("**")) * pw * lpeg.V("factor"))^-1)),
    atomExpr =
        atomExprInit(lpeg.V("atom") * lpeg.Ct(lpeg.V("trailer")^0)),
    atom =
        numberLiteralInit(numberLiteral) +
        stringLiteralInit(stringLiteral) +
        lpeg.V("vectorLiteral") +
        lpeg.V("dictOrSetLiteral") +
        moduleMemberOrKeywordArg(
            lpeg.V("name") *
            (lpeg.V("keywordArgTrailer") + lpeg.V("moduleMemberTrailer"))^-1) +
        lpeg.P("(") * lpeg.V("expr") * lpeg.P(")") +
        refInit(lpeg.P("ref") * pw * lpeg.V("subExpr")),
    trailer =
        functionTrailer(lpeg.P("(") * lpeg.V("argList") * lpeg.P(")")) +
        lpeg.P("[") * lpeg.V("sliceOrSubscript") * lpeg.P("]") +
        structFieldTrailer(lpeg.P(".") * lpeg.V("name")) +
        unwrapTrailer(lpeg.Cs(lpeg.P("?"))),
    comparisonOp =
        lpeg.P("==") + lpeg.P("<=") + lpeg.P(">=") + lpeg.P("!=") +
        lpeg.P("<") + lpeg.P(">") + lpeg.P("in") + lpeg.P("is"),
    mulDivOp = lpeg.P("*") + lpeg.P("/"),
    addSubOp = lpeg.P("+") + lpeg.P("-"),
    sliceOrSubscript =
        sliceOrSubscriptInit(lpeg.V("expr") * lpeg.V("sliceTrailer")),
    sliceTrailer =
        lpeg.Cs(lpeg.P("...") + lpeg.P("..<")) * lpeg.V("expr") *
        lpeg.V("possibleSliceStep") +
        lpeg.P(""),
    possibleSliceStep = lpeg.P(":") * lpeg.V("expr") + lpeg.P(""),

    type = typeDependencyInit(
            lpeg.V("typeExpr") * optNil(pw * lpeg.P("is") * pw * lpeg.P("(") *
                lpeg.V("types") * lpeg.P(")"))^-1),
    typeExpr = maybeInit(
        lpeg.V("typeTerm") * lpeg.V("questionMark")),
    typeTerm = typeCombInit(
        lpeg.V("typeFactor") *
        (pw * lpeg.V("typeCombOp") * pw * lpeg.V("type"))^0),
    typeFactor = parameterizedTypeInit(
        lpeg.V("typeAtom") *
        optNil((lpeg.P("(") * lpeg.V("types") * lpeg.P(")"))^-1)),
    typeAtom =
        refInit(lpeg.P("ref") * pw * lpeg.V("typeFactor")) +
        lpeg.P("(") * lpeg.V("type") * lpeg.P(")") +
        moduleMemberOrKeywordArg(lpeg.V("name") * lpeg.V("moduleMemberTrailer")^-1),

    typeCombOp = lpeg.Cs(lpeg.P("and") + lpeg.P("or")),
    questionMark = lpeg.Cs(lpeg.P("?")) + lpeg.P(""),
})


function parse(input)
    return grammar:match(input)
end


return {parse = parse}
