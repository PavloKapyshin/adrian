module Adrian.Madgo.Parser where


import Text.Parsec (parse, many, many1, eof, optional, try)
import Text.Parsec.Char (char, string, oneOf, lower, upper, letter, digit)
import Text.Parsec.Expr (buildExpressionParser, Operator(Infix), Assoc(AssocLeft))
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Control.Applicative (
    (<*>), (<*), (*>), (<$>), (<|>), (<$), liftA, liftA2, liftA3)
import Control.Monad (void)

import qualified Adrian.Madgo.AST as AST


-- Parse whitespaces: spaces, newlines, tabs.
whitespaces :: Parser ()
whitespaces = void $ many $ oneOf " \n\t"


-- Parse something that can have whitespaces around.
lexeme :: Parser a -> Parser a
lexeme parser = whitespaces *> (parser <* whitespaces)


-- Parse Adrian's module name.
moduleNameParser :: Parser AST.ModuleName
moduleNameParser = many1 (letter <|> char '_')


-- Parse Adrian's name.
-- Just parsing first char and then parsing the rest of string.
-- in this case: (lower :: Char) (many :: Char -> [Char])
-- (liftA :: (String -> AST.Name) -> Parser String -> Parser AST.Name)
nameParser :: Parser AST.Name
nameParser = liftA
    AST.Name (((:) <$> lower <*> many (letter <|> digit)) :: Parser String)


-- Parse Adrian's type name.
-- Right: Type  MyType  LOL
-- Wrong: type  myType  l_o_l
typeNameParser :: Parser AST.Type
typeNameParser = liftA
    AST.Type (((:) <$> upper <*> many (letter <|> digit)) :: Parser String)


-- Parse Adrian's type from module.
-- Typical example: some_module#SomeType
-- There are no whitespaces around hash char.
-- There are no modules inside of other modules so 
-- you can't write something like `c#another_module#Type`.
typeFromModuleParser :: Parser AST.Type
typeFromModuleParser = liftA2
    AST.TypeFromModule (moduleNameParser <* char '#') typeNameParser


-- Parse Adrian's type.
typeParser :: Parser AST.Type
typeParser = ((try typeFromModuleParser) <|> typeNameParser)


-- Parse integer literal.
integerLiteralParser :: Parser AST.Expr
integerLiteralParser = liftA AST.IntegerLiteral $ many1 digit


-- Recursively parse arguments.
-- Argument == expression.
-- Don't use this function for parsing declaration arguments.
--
--              parsing these args
--          vvvvvvvvvvvvvvvvvvvvvvvvvv
-- someFunc(10, anotherFunc(), 30 + 40)
--
--           for these use declarationArgsParser (not written)
--             vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
-- fun newFunc(someArg: c#IntFast8; anotherOne: c#IntFast32)
argsParser :: Parser AST.Args
argsParser = (
    -- parsing a lot of arguments.
    try ((:) <$> (exprParser <* char ',' <* whitespaces) <*> argsParser)) <|>
    ((:) <$> exprParser <*> pure []) <|>    -- parsing one argument.
    (pure [])   -- parsing empty.


-- Parse Adrian's struct call.
-- Example: MyMegaStruct(10, 20, someFunc())
structCallParser :: Parser AST.Expr
structCallParser = liftA2
    AST.StructCall typeParser (char '(' *> argsParser <* char ')')


operatorTable = [
    [Infix ((AST.SExpr "*") <$ (try $ lexeme $ string "*")) AssocLeft,
     Infix ((AST.SExpr "/") <$ (try $ lexeme $ string "/")) AssocLeft],

    [Infix ((AST.SExpr "+") <$ (try $ lexeme $ string "+")) AssocLeft,
     Infix ((AST.SExpr "-") <$ (try $ lexeme $ string "-")) AssocLeft]
    ]


termParser :: Parser AST.Expr
termParser = (structCallParser <|> integerLiteralParser)


-- Parse Adrian's expression, struct call or integer iteral for now.
exprParser :: Parser AST.Expr
exprParser = buildExpressionParser operatorTable termParser


-- Parse Adrian's variable declaration statement.
declarationParser :: Parser AST.Node
declarationParser = liftA3
    AST.VariableDeclaration ((lexeme $ string "var") *> nameParser)
    ((lexeme $ char ':') *> typeParser) ((lexeme $ char '=') *> exprParser)


-- Parse Adrian's source code and returns AST.
astParser :: Parser AST.AST
astParser = (many $ lexeme declarationParser) <* eof


-- Wrapper around astParser. Use it instead of astParser.
parseSourceCode :: String -> Either ParseError AST.AST
parseSourceCode source_code = parse astParser "" source_code
