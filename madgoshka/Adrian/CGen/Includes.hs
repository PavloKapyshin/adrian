module Adrian.CGen.Includes where

import Adrian.CGen.AST


data Include = Include String deriving (Eq, Ord)


stdint :: Include
stdint = Include "stdint.h"


typeToIncludes :: Type -> [Include]
typeToIncludes IntFast8 = [stdint]
typeToIncludes _ = []
