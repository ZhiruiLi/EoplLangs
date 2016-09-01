module LetLang.Data.ExpressedValue where

data ExpressedValue = NumVal Integer
                    | BoolVal Bool
                    | ListVal ExpressedList
  deriving (Eq)

data ExpressedList = NonEmpty ExpressedValue ExpressedList
                   | Empty
  deriving (Eq)

instance Show ExpressedList where
  show Empty=             "()"
  show (NonEmpty h Empty) = "(" `mappend` show h `mappend` ")"
  show (NonEmpty h t) =
    "(" `mappend` show h `mappend` showT t `mappend` ")"
    where
      showT Empty          = ""
      showT (NonEmpty h t) = " " `mappend` show h `mappend` showT t

instance Show ExpressedValue where
  show = showExpVal

showExpVal :: ExpressedValue -> String
showExpVal (NumVal i)    = show i
showExpVal (BoolVal b)   = show b
showExpVal (ListVal lst) = show lst
