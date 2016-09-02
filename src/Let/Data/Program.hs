module Let.Data.Program where

import           Let.Data.Expression

data Program = Program Expression
  deriving (Show, Eq)
