module Adrian.Madgo.Env where


import qualified Data.Map as M

import qualified Adrian.Madgo.AST as AST


type Entity = M.Map String AST.Type
type Env = M.Map String Entity
type Envs = [Env]


type Options = M.Map String String
type Context = (Envs, Options)


-- Lookup for a name in envs from left to right,
--
-- if we have envs like this: [{"n": 1}, {"n": 2}]
-- the result of `_lookupName thisTables "n"` will be 1.
--
-- if name is not an element of any of the envs then
-- Nothing will be returned
_lookupName :: Envs -> String -> Maybe Entity
_lookupName [] _ = Nothing
_lookupName (env:envs) name =
    case M.lookup name env of
        Just entity -> Just entity
        Nothing -> _lookupName envs name


getName :: Envs -> String -> Maybe Entity
getName envs name = _lookupName envs name


-- Add name to the first env.
addName :: Envs -> String -> Entity -> M.Map String Entity
addName (env:_) name entity = M.insert name entity env


fromList :: Ord k => [(k, a)] -> M.Map k a
fromList = M.fromList


emptyEnv :: Env
emptyEnv = M.empty
