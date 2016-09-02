module LetLang.Data.ExpressedValue where

data ExpressedValue = NumVal Integer
                    | BoolVal Bool
                    | ListVal [ExpressedValue]
  deriving (Eq)

instance Show ExpressedValue where
  show = showExpVal

showExpVal :: ExpressedValue -> String
showExpVal (NumVal i)    = show i
showExpVal (BoolVal b)   = show b
showExpVal (ListVal lst) = showListVal lst

showListVal :: [ExpressedValue] -> String
showListVal []     =             "()"
showListVal [h] = "(" `mappend` show h `mappend` ")"
showListVal (h:t) = "(" `mappend` show h `mappend` showTail t `mappend` ")"
  where
    showTail = foldr (\h -> mappend (" " `mappend` show h)) ""

