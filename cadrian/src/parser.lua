package.cpath = package.cpath .. ";libs/lpeg/?.so"
local lpeg = require("lpeg")


local lowercaseLetter = lpeg.Cs(lpeg.R("az"))
local uppercaseLetter = lpeg.Cs(lpeg.R("AZ"))
local letter = lpeg.Cs(lowercaseLetter + uppercaseLetter)
local number = lpeg.Cs(lpeg.R("09"))
local underscore = lpeg.Cs("_")
local pW = lpeg.S(" \n")^0

local keywords = "let"

local numberLiteral = lpeg.Cs(number^1)
local stringLiteral = lpeg.Cs(
    lpeg.P('"') * (lpeg.P(1) - '"')^0 * lpeg.P('"'))
local name = lpeg.Cs(
    (letter + underscore) * (letter + underscore + number)^0 - keywords)
local expr = numberLiteral + stringLiteral

local letDeclaration = lpeg.Cs(
    pW * lpeg.P("let") * pW * name * pW * lpeg.P("=") * pW * expr * pW)

local statement = letDeclaration
local ast = lpeg.Cs(statement^1) + ""


print(letDeclaration:match("let some = 1"))
print(letDeclaration:match('let other = "hello, world!"'))
