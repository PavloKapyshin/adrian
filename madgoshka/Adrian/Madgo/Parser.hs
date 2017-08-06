module Adrian.Madgo.Parser where


import Text.Parsec (parse, many, many1, eof, optional, try)
import Text.Parsec.Char (char, string, oneOf, lower, upper, letter, digit)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Control.Applicative ((<*>), (<*), (*>), (<$>), (<|>), liftA, liftA2, liftA3)
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


-- Parse Adrian's expression, only integer literal for now.
integerLiteralParser :: Parser AST.Expr
integerLiteralParser = liftA AST.IntegerLiteral $ many1 digit


argsParser :: Parser AST.Args
argsParser = (try ((:) <$> (exprParser <* char ',' <* whitespaces) <*> argsParser)) <|> ((:) <$> exprParser <*> pure []) <|> (pure [])


structCallParser :: Parser AST.Expr
structCallParser = liftA2 AST.StructCall typeParser (char '(' *> argsParser <* char ')')


-- Parse Adrian's expression, only integer literal for now.
exprParser :: Parser AST.Expr
exprParser = (structCallParser <|> integerLiteralParser)


-- Parse Adrian's variable declaration statement.
declarationParser :: Parser AST.Node
declarationParser = liftA3
    AST.VariableDeclaration ((lexeme $ string "var") *> nameParser)
    ((lexeme $ string ":") *> typeParser) ((lexeme $ string "=") *> exprParser)


-- Parse Adrian's source code and returns AST.
astParser :: Parser AST.AST
astParser = (many $ lexeme declarationParser) <* eof


-- Wrapper around astParser. Use it instead of astParser.
parseSourceCode :: String -> Either ParseError AST.AST
parseSourceCode source_code = parse astParser "" source_code
