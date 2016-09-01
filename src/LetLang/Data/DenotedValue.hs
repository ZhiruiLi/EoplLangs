module LetLang.Data.DenotedValue where

data DenotedValue = NumVal Integer | BoolVal Bool
  deriving (Eq)

instance Show DenotedValue where
  show = showDenVal

showDenVal :: DenotedValue -> String
showDenVal (NumVal i)  = show i
showDenVal (BoolVal b) = show b
