package.cpath = package.cpath .. ";libs/lpeg/?.so"
local lpeg = require("lpeg")
package.path = package.path .. ";src/?.lua"
local astlib = require("astlib")

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


local function toStructPath(p)
    return p / function (path)
        return astlib.StructPath.new(path)
    end
end



local function keywordArg(p)
    return p / function (name, expr)
        if expr == nil then
            return name
        end
        return astlib.KeywordArg.new(name, expr)
    end
end


local function parameterizedTypeInit(p)
    return p / function (base, parameters)
        if parameters == nil then
            return base
        end
        return astlib.ParameterizedType.new(base, parameters)
    end
end

local function typeCombInit(p)
    return p / function (left, op, right)
        if op == nil then
            return left
        end
        return astlib.TypeCombination.new(left, op, right)
    end
end

local function refTypeInit(p)
    return p / function (base)
        return astlib.Ref.new(base)
    end
end

local function maybeInit(p)
    return p / function (base, mark)
        if mark == nil then
            return base
        end
        return astlib.Maybe.new(base)
    end
end

local function typeDependencyInit(p)
    return p / function (base, protocols)
        if protocols == nil then
            return base
        end
        return astlib.TypeDependency.new(base, protocols)
    end
end


local function nameInit(p)
    return p / function (string)
        return astlib.Name.new(string)
    end
end

local function vectorLiteralInit(p)
    return p / function (text)
        return astlib.Literal.new(astlib.literalTypes.vector, text)
    end
end

local function dictOrSetLiteralInit(p)
    return p / function (text)
        return astlib.Literal.new(astlib.literalTypes.dictOrSet, text)
    end
end

local function numberLiteralInit(p)
    return p / function (text)
        return astlib.Literal.new(astlib.literalTypes.number, text)
    end
end

local function stringLiteralInit(p)
    return p / function (text)
        return astlib.Literal.new(astlib.literalTypes.string, text)
    end
end

local function letDeclInit(p)
    return p / function (name, type, expr)
        return astlib.LetDeclaration.new(name, type, expr)
    end
end

local function varDeclInit(p)
    return p / function (name, type, expr)
        return astlib.VarDeclaration.new(name, type, expr)
    end
end

local function funcProtoInit(p)
    return p / function (name, args, type)
        return astlib.FuncPrototype.new(name, args, type)
    end
end

local function funcDeclInit(p)
    return p / function (funcProto, body)
        return astlib.FuncDeclaration.new(
            funcProto.name, funcProto.args, funcProto.type, body)
    end
end

local function argumentInit(p)
    return p / function (name, type, expr)
        return astlib.Argument.new(name, type, expr)
    end
end

local function protoDeclInit(p)
    return p / function (name, parameters, protocols, body)
        return astlib.ProtocolDeclaration.new(name, parameters, protocols, body)
    end
end

local function structDeclInit(p)
    return p / function (name, parameters, protocols, body)
        return astlib.StructDeclaration.new(name, parameters, protocols, body)
    end
end

local function extDeclInit(p)
    return p / function (name, parameters, protocols, body)
        return astlib.ExtensionDeclaration.new(name, parameters, protocols, body)
    end
end

local function reassignmentInit(p)
    return p / function (left, op, right)
        return astlib.Reassignment.new(left, op, right)
    end
end

local function forStmtInit(p)
    return p / function (names, container, body)
        return astlib.For.new(names, container, body)
    end
end

local function whileStmtInit(p)
    return p / function (expr, body)
        return astlib.While.new(expr, body)
    end
end

local function condStmtInit(p)
    return p / function (expr, body, elifs, elseStmt)
        return astlib.Cond.new(expr, body, elifs, elseStmt)
    end
end

local function elifStmtInit(p)
    return p / function (expr, body)
        return astlib.Elif.new(expr, body)
    end
end

local function elseStmtInit(p)
    return p / function (body)
        return astlib.Else.new(body)
    end
end

local function breakStmtInit(p)
    return p / function (input)
        return astlib.Break.new()
    end
end

local function returnInit(p)
    return p / function (expr)
        return astlib.Return.new(expr)
    end
end

local function fieldDeclInit(p)
    return p / function (name, type)
        return astlib.FieldDeclaration.new(name, type)
    end
end

local function moduleMemberInit(p)
    return p / function (moduleName, member)
        return astlib.ModuleMember.new(moduleName, member)
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
        if luatype(someValue) ~= "table" then
            return nil
        end
        if getmetatable(someValue) == nil and #someValue == 0 then
            return nil
        end
        return someValue
    end
end

local function optStr(p, string)
    return p / function (someValue)
        if luatype(someValue) ~= "string" then
            return nil
        elseif string == someValue then
            return someValue
        end
        return nil
    end
end


local function functionTrailer(p)
    return p / function (argList)
        return astlib.Call.new(argList)
    end
end

local function subscriptTrailer(p)
    return p / function (subs)
        return subs
    end
end

local function structFieldTrailer(p)
    return p / function (fieldName)
        return astlib.Field(fieldName)
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
        if #trailers == 0 then
            return atom
        end
        return astlib.TraileredExpr.new(atom, trailers)
    end
end

local function sliceOrSubscriptInit(p)
    return p / function (left, rangeOp, right, step)
        if rangeOp == nil then
            return astlib.Subscript.new(left)
        end
        return astlib.Slice.new(left, rangeOp, right, step)
    end
end

local function unwrapTrailer(p)
    return p / function (exclamationMark)
        if exclamationMark ~= nil and exclamationMark == "!" then
            return astlib.Unwrap.new()
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
        return astlib.Expr.new(left, other[1], other[2])
    else
        return astlib.Expr.new(
            left, other[1], _makeExpr(other[2], table.slice(other, 3)))
    end
end

local function makeExpr(p)
    return p / _makeExpr
end

local function unaryOperator(p)
    return p / function (op, expr)
        return astlib.UnaryOp.new(op, expr)
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

    keywordArgTrailer = pw * lpeg.P("=") * pw * lpeg.V("expr") + lpeg.P(""),
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
    moduleMember = moduleMemberInit(
        lpeg.V("name") * lpeg.P("#") * lpeg.V("name")),
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
        keywordArg(lpeg.V("name") * lpeg.V("keywordArgTrailer")) +
        lpeg.V("moduleMember") +
        lpeg.P("(") * lpeg.V("expr") * lpeg.P(")"),
    trailer =
        functionTrailer(lpeg.P("(") * lpeg.V("argList") * lpeg.P(")")) +
        lpeg.P("[") * lpeg.V("sliceOrSubscript") * lpeg.P("]") +
        structFieldTrailer(lpeg.P(".") * lpeg.V("name")) +
        unwrapTrailer(lpeg.Cs(lpeg.P("!"))),
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
        refTypeInit(lpeg.P("ref") * pw * lpeg.V("typeFactor")) +
        lpeg.P("(") * lpeg.V("type") * lpeg.P(")") +
        lpeg.V("moduleMember") + lpeg.V("name"),

    typeCombOp = lpeg.Cs(lpeg.P("and") + lpeg.P("or")),
    questionMark = lpeg.Cs(lpeg.P("?")) + lpeg.P(""),
})


local input = [[
    let a = not a
]]


for _, v in ipairs(grammar:match(input)) do
    print(v)
end
