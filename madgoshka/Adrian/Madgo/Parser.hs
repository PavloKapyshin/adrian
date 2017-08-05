module Adrian.Madgo.Parser where


import Text.Parsec (parse, many, many1, eof, optional)
import Text.Parsec.Char (string, oneOf, lower, upper, letter, digit)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Control.Applicative ((<*>), (<*), (*>), (<$>), (<|>), liftA, liftA3)
import Control.Monad (void)

import qualified Adrian.Madgo.AST as AST


-- Parse whitespace: spaces, newlines, tabs.
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"


-- Parse something that can have whitespaces around.
lexeme :: Parser a -> Parser a
lexeme parser = whitespace *> (parser <* whitespace)


-- Parse Adrian's name.
-- Just parsing first char and then parsing the rest of string.
-- in this case: (lower :: Char) (many :: Char -> [Char])
-- (liftA :: (String -> AST.Name) -> Parser String -> Parser AST.Name)
nameParser :: Parser AST.Name
nameParser = liftA
    AST.Name (((:) <$> lower <*> many (letter <|> digit)) :: Parser String)


-- Parse Adrian's type.
typeParser :: Parser AST.Type
typeParser = liftA
    AST.Type (((:) <$> upper <*> many (letter <|> digit)) :: Parser String)


-- Parse Adrian's expression, only integer literal for now.
exprParser :: Parser AST.Expr
exprParser = liftA AST.IntegerLiteral $ many1 digit


-- Parse Adrian's variable declaration statement.
declarationParser :: Parser AST.Node
declarationParser = liftA3
    AST.VariableDeclaration ((lexeme $ string "var") *> nameParser)
    ((lexeme $ string ":") *> typeParser) ((lexeme $ string "=") *> exprParser)


astParser :: Parser AST.AST
astParser = (many $ lexeme declarationParser) <* eof


-- Parse Adrian source code and returns AST.
parseSourceCode :: String -> Either ParseError AST.AST
parseSourceCode source_code = parse astParser "" source_code
