module LetLang.Data.Program where

import           LetLang.Data.Expression

data Program = Program Expression
  deriving (Show, Eq)
