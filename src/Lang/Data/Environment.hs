module Lang.Data.Environment
( Environment
, empty
, initEnvironment
, extend
, extendMany
, apply
, applySafe
) where

import qualified Data.Map                 as M
import           Data.Maybe               (fromMaybe)
import           Lang.Data.ExpressedValue

type Environment = M.Map String ExpressedValue

empty :: Environment
empty = M.empty

initEnvironment :: [(String, ExpressedValue)] -> Environment
initEnvironment = M.fromList

extend :: String -> ExpressedValue -> Environment -> Environment
extend = M.insert

extendMany :: [(String, ExpressedValue)] -> Environment -> Environment
extendMany list env = M.union env $ M.fromList list

apply :: Environment -> String -> ExpressedValue
apply env var = fromMaybe
  (error $ "Var " `mappend` var `mappend` " is not in environment!")
  (applySafe env var)

applySafe :: Environment -> String -> Maybe ExpressedValue
applySafe = flip M.lookup

