-- | Template haskell helper functions.

module TH ( getEnvVariableExpr ) where

import           Language.Haskell.TH        (Exp, Q, runIO)
import           Language.Haskell.TH.Syntax (lift)
import           System.Environment         (getEnvironment)

getEnvVariableExpr :: String -> Q Exp
getEnvVariableExpr envKey = do
    env <- runIO getEnvironment
    case lookup envKey env of
        Nothing -> error $ "Environment variable " ++ envKey ++ " not defined."
        Just value -> lift value

