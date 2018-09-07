package.cpath = package.cpath .. ";libs/lpeg/?.so"
package.path = package.path .. ";src/?.lua"

local astlib = require("astlib")
local context = require("context")
local parser = require("parser")
local desugar = require("desugar")


local function readFile(fileName)
    local file = io.open(fileName, "r")
    return file:read("a")
end


local function main(fileName)
    local contents = readFile(fileName)
    local ast = parser.parse(contents)
    local context = context.Context.new({"library/"})
    for _, layer in ipairs({desugar}) do
        ast = layer.main(ast, context)
        context = context:copy()
    end
    for _, v in ipairs(ast) do
        print(v:toCode())
    end
end


local fileName = arg[1]
if fileName == nil then
    error("no Adrian source file to open", 0)
end

main(arg[1])
