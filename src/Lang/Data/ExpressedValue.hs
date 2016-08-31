module Lang.Data.ExpressedValue where

data ExpressedValue = NumVal Integer | BoolVal Bool
  deriving (Eq)

instance Show ExpressedValue where
  show = showExpVal

showExpVal :: ExpressedValue -> String
showExpVal (NumVal i)  = show i
showExpVal (BoolVal b) = show b
