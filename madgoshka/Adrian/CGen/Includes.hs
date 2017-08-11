module Adrian.CGen.Includes where

import Adrian.CGen.AST


stdint :: Include
stdint = Include "stdint.h"

stdlib :: Include
stdlib = Include "stdlib.h"


typeToIncludes :: Type -> [Include]
typeToIncludes IntFast8 = [stdint]
typeToIncludes _ = []
