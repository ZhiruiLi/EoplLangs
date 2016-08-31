module Lang.Data.Program where

import           Lang.Data.Expression

data Program = Program Expression
  deriving (Show, Eq)
