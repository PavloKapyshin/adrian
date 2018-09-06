local function joinWithConversion(t, before, after)
    local result = ""
    for _, v in ipairs(t) do
        result = result .. before .. tostring(v) .. after
    end
    return result
end


local indentationLevel = 0


local function repeatString(string, times)
    local result = ""
    for i = times, 1, -1 do
        result = result .. string
    end
    return result
end


local function joinBody(body)
    local result = "{\n"
    indentationLevel = indentationLevel + 1
    for _, v in ipairs(body) do
        result =
            result .. repeatString("  ", indentationLevel) .. tostring(v) .. "\n"
    end
    indentationLevel = indentationLevel - 1
    return result .. repeatString("  ", indentationLevel) .. "}"
end


Expr = {}

function Expr.new(left, op, right)
    return setmetatable({left = left, op = op, right = right}, Expr)
end

function Expr:__tostring()
    return string.format("(%s %s %s)", self.left, self.op, self.right)
end


UnaryOp = {}

function UnaryOp.new(op, expr)
    return setmetatable({op = op, expr = expr}, UnaryOp)
end

function UnaryOp:__tostring()
    return string.format("(%s %s)", self.op, self.expr)
end


TraileredExpr = {}

function TraileredExpr.new(base, trailers)
    return setmetatable({base = base, trailers = trailers}, TraileredExpr)
end

function TraileredExpr:__tostring()
    return string.format(
        "T(%s, (%s))", self.base, joinWithConversion(self.trailers, "", ", "))
end


Call = {}

function Call.new(args)
    return setmetatable({args = args}, Call)
end

function Call:__tostring()
    return string.format(
        "C(%s)", joinWithConversion(self.args, "", ", "))
end


Field = {}

function Field.new(name)
    return setmetatable({name = name}, Field)
end

function Field:__tostring()
    return string.format("F(%s)", self.name)
end


Slice = {}

function Slice.new(lowerBound, op, upperBound, step)
    return setmetatable(
        {lowerBound = lowerBound, op = op, upperBound = upperBound, step = step},
        Slice)
end

function Slice:__tostring()
    return string.format(
        "Sl(%s%s%s:%s)", self.lowerBound, self.op, self.upperBound, self.step)
end


Subscript = {}

function Subscript.new(value)
    return setmetatable({value = value}, Subscript)
end

function Subscript:__tostring()
    return string.format("Sb(%s)", self.value)
end


KeywordArg = {}

function KeywordArg.new(name, expr)
    return setmetatable({name = name, expr = expr}, KeywordArg)
end

function KeywordArg:__tostring()
    return string.format("%s = %s", self.name, self.expr)
end


ParameterizedType = {}

function ParameterizedType.new(base, parameters)
    return setmetatable(
        {base = base, parameters = parameters}, ParameterizedType)
end

function ParameterizedType:__tostring()
    return string.format(
        "%s(%s)", self.base, joinWithConversion(self.parameters, "", ", "))
end


TypeCombination = {}

function TypeCombination.new(left, op, right)
    return setmetatable(
        {left = left, op = op, right = right}, TypeCombination)
end

function TypeCombination:__tostring()
    return string.format("%s %s %s", self.left, self.op, self.right)
end


Ref = {}

function Ref.new(base)
    return setmetatable({base = base}, Ref)
end

function Ref:__tostring()
    return string.format("ref(%s)", self.base)
end


Maybe = {}

function Maybe.new(base)
    return setmetatable({base = base}, Maybe)
end

function Maybe:__tostring()
    return string.format("(%s)?", self.base)
end


Unwrap = {}

function Unwrap.new()
    return setmetatable({}, Unwrap)
end

function Unwrap:__tostring()
    return "(!)"
end


TypeDependency = {}

function TypeDependency.new(base, protocols)
    return setmetatable({base = base, protocols = protocols}, TypeDependency)
end

function TypeDependency:__tostring()
    return string.format(
        "(%s is (%s))", self.base, joinWithConversion(self.protocols, "", ", "))
end


literalTypes = {
    number = "number",
    string = "string",
    vector = "vector",
    dictOrSet = "dictOrSet",
}

Literal = {}

function Literal.new(type, text)
    return setmetatable({type = type, text = text}, Literal)
end

function Literal:__tostring()
    if self.type == literalTypes.vector or
            self.type == literalTypes.dictOrSet then
        local text = joinWithConversion(self.text, "", ", ")
    else
        local text = self.text
    end
    return string.format("L(%s: %s)", self.text, self.type)
end


Name = {}

function Name.new(string)
    return setmetatable({string = string}, Name)
end

function Name:__tostring()
    return string.format("%s", self.string)
end


ModuleMember = {}

function ModuleMember.new(moduleName, member)
    return setmetatable(
        {moduleName = moduleName, member = member}, ModuleMember)
end

function ModuleMember:__tostring()
    return string.format("%s#%s", self.moduleName, self.member)
end


Argument = {}

function Argument.new(names, type, defaultExpr)
    return setmetatable(
        {names = names, type = type, defaultExpr = defaultExpr}, Argument)
end

function Argument:__tostring()
    return string.format(
        "%s: %s = %s", joinWithConversion(self.names, "", " "),
        self.type, self.defaultExpr)
end


FieldDeclaration = {}

function FieldDeclaration.new(name, type)
    return setmetatable({name = name, type = type}, FieldDeclaration)
end

function FieldDeclaration:__tostring()
    return string.format("%s: %s", self.name, self.type)
end


Return = {}

function Return.new(expr)
    return setmetatable({expr = expr}, Return)
end

function Return:__tostring()
    return string.format("return %s", self.expr)
end


ProtocolDeclaration = {}

function ProtocolDeclaration.new(name, parameters, protocols, body)
    return setmetatable(
        {name = name, parameters = parameters, protocols = protocols, body = body},
        ProtocolDeclaration)
end

function ProtocolDeclaration:__tostring()
    return string.format(
        "protocol %s(%s) is (%s) %s\n",
        self.name, joinWithConversion(self.parameters, "", ", "),
        joinWithConversion(self.protocols, "", ", "),
        joinBody(self.body))
end


StructDeclaration = {}

function StructDeclaration.new(name, parameters, protocols, body)
    return setmetatable(
        {name = name, parameters = parameters, protocols = protocols, body = body},
        StructDeclaration)
end

function StructDeclaration:__tostring()
    return string.format(
        "struct %s(%s) is (%s) %s\n",
        self.name, joinWithConversion(self.parameters, "", ", "),
        joinWithConversion(self.protocols, "", ", "),
        joinBody(self.body))
end


ExtensionDeclaration = {}

function ExtensionDeclaration.new(name, parameters, protocols, body)
    return setmetatable(
        {name = name, parameters = parameters, protocols = protocols, body = body},
        ExtensionDeclaration)
end

function ExtensionDeclaration:__tostring()
    return string.format(
        "extension %s(%s) is (%s) %s\n",
        self.name, joinWithConversion(self.parameters, "", ", "),
        joinWithConversion(self.protocols, "", ", "),
        joinBody(self.body))
end


Reassignment = {}

function Reassignment.new(left, op, right)
    return setmetatable(
        {left = left, op = op, right = right}, Reassignment)
end

function Reassignment:__tostring()
    return string.format("%s %s %s", self.left, self.op, self.right)
end


For = {}

function For.new(names, container, body)
    return setmetatable(
        {names = names, container = container, body = body}, For)
end

local function joinNames(input)
    local names = ""
    for _, v in ipairs(input) do
        if getmetatable(v) ~= Name then
            names = names .. "(" .. joinNames(v) .. ")"
        else
            names = names .. tostring(v) .. ", "
        end
    end
    return names
end

function For:__tostring()
    return string.format(
        "for (%s) in %s %s",
        joinNames(self.names), self.container, joinBody(self.body))
end


While = {}

function While.new(expr, body)
    return setmetatable({expr = expr, body = body}, While)
end

function While:__tostring()
    return string.format("while %s %s", self.expr, joinBody(self.body))
end


Cond = {}

function Cond.new(expr, body, elifs, elseStmt)
    return setmetatable(
        {expr = expr, body = body, elifs = elifs, elseStmt = elseStmt}, Cond)
end

function Cond:__tostring()
    local elseStmt = self.elseStmt or ""
    return string.format(
        "if %s %s%s %s",
        self.expr, joinBody(self.body),
        joinWithConversion(self.elifs, " ", ""), elseStmt)
end


Elif = {}

function Elif.new(expr, body)
    return setmetatable({expr = expr, body = body}, Elif)
end

function Elif:__tostring()
    return string.format("elif %s %s", self.expr, joinBody(self.body))
end

Else = {}

function Else.new(body)
    return setmetatable({body = body}, Else)
end

function Else:__tostring()
    return string.format("else %s", joinBody(self.body))
end


Break = {}

function Break.new()
    return setmetatable({}, Break)
end

function Break:__tostring()
    return "break"
end


LetDeclaration = {}

function LetDeclaration.new(name, type, expr)
    return setmetatable(
        {name = name, type = type, expr = expr}, LetDeclaration)
end

function LetDeclaration:__tostring()
    return string.format(
        "let %s: %s = %s", self.name, self.type, self.expr)
end


VarDeclaration = {}

function VarDeclaration.new(name, type, expr)
    return setmetatable(
        {name = name, type = type, expr = expr}, VarDeclaration)
end

function VarDeclaration:__tostring()
    return string.format(
        "var %s: %s = %s", self.name, self.type, self.expr)
end


FuncPrototype = {}

function FuncPrototype.new(name, args, type)
    return setmetatable({name = name, args = args, type = type}, FuncPrototype)
end

function FuncPrototype:__tostring()
    return string.format(
        "fun %s(%s): %s", self.name, joinWithConversion(self.args, "", ", "),
        self.type)
end


FuncDeclaration = {}

function FuncDeclaration.new(name, args, type, body)
    return setmetatable(
        {name = name, args = args, type = type, body = body}, FuncDeclaration)
end

function FuncDeclaration:__tostring()
    return string.format(
        "fun %s(%s): %s %s\n",
        self.name, joinWithConversion(self.args, "", ", "), self.type,
        joinBody(self.body))
end

return _G
