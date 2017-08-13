module Adrian.CGen.Includes where

import Adrian.CGen.AST


stdint :: Include
stdint = Include "stdint.h"

stdlib :: Include
stdlib = Include "stdlib.h"


typeToIncludes :: Type -> [Include]
typeToIncludes UIntFast8 = [stdint]
typeToIncludes IntFast8 = [stdint]
typeToIncludes UIntFast32 = [stdint]
typeToIncludes IntFast32 = [stdint]
typeToIncludes _ = []
