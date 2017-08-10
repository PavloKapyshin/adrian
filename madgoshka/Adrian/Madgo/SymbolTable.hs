module Adrian.Madgo.SymbolTable where


import qualified Data.Map as M

import qualified Adrian.Madgo.AST as AST


type Entity = M.Map String AST.Type
type Table = M.Map String Entity
type Tables = [Table]


type CompilerOptions = M.Map String String
type Context = (Tables, CompilerOptions)


-- Lookup for a name in tables from left to right,
--
-- if we have tables like this: [{"n": 1}, {"n": 2}]
-- the result of `_lookupName thisTables "n"` will be 1.
--
-- if name is not an element of any of the tables then
-- Nothing will be returned
_lookupName :: Tables -> String -> Maybe Entity
_lookupName [] _ = Nothing
_lookupName (table:tables) name =
    case M.lookup name table of
        Just entity -> Just entity
        Nothing -> _lookupName tables name


getName :: Context -> String -> Maybe Entity
getName (tables, _) name = _lookupName tables name


-- Add name to the first table.
addName :: Context -> String -> Entity -> M.Map String Entity
addName (table:_, _) name entity = M.insert name entity table


fromList :: Ord k => [(k, a)] -> M.Map k a
fromList = M.fromList


emptyTable :: Table
emptyTable = M.empty
