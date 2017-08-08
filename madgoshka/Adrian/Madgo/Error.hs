module Adrian.Madgo.Error where


type Message = String

data CompilationError = CompilationError Message
    deriving (Show)
