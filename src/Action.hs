module Action where

import Control.Monad hiding (forever)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import qualified System.Log.Logger as Log
import System.Log.Logger (Priority(..))

newtype Error = Error String deriving (Eq, Show)

instance Monoid Error where
    mempty = Error ""
    Error e1 `mappend` Error e2 = Error $ e1 ++ e2

type Action a = ExceptT Error IO a

runAction :: ExceptT e m a -> m (Either e a)
runAction = runExceptT

(!>) :: Action Bool -> String -> Action ()
(!>) act err = do
    rv <- act
    when (not rv) $ throwE $ Error err

(?>) :: Action (Maybe a) -> String -> Action a
(?>) act err = do
    mval <- act
    case mval of
        Nothing -> throwE $ Error err
        Just val -> return val

-- | Throw exception.
throw :: String -> Action a
throw err = throwE $ Error err

-- | Assert True.
assert :: Bool -> String -> Action ()
assert condition err = case condition of
    True -> return ()
    False -> throw err

-- | Log message.
logM :: String -> Priority -> String -> Action ()
logM func level msg = do
    liftIO $ Log.logM ("vcr."++func) level msg

-- | Dump debug message.
trace :: String -> Action ()
trace = logM "debug" DEBUG

nop :: Action ()
nop = return ()

forever :: Monad m => m a -> m b
forever act = do
    _ <- act
    forever act

