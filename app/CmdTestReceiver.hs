-- | Simple UDP receiver - for test purposes.

module CmdTestReceiver where

-- standard imports
import           Control.Concurrent        (threadDelay)
import           Control.Monad
import           Control.Monad.Fix
import qualified Data.ByteString           as BS
import           GHC.Generics              (Generic)
import qualified Network.Multicast         as Mcast
import qualified Network.Socket            as Net
import qualified Network.Socket.ByteString as NB
import           Options.Applicative
import           UnliftIO

-- local imports
import           Common

type Host = String
type Mcast = String
type Ip = String
type Port = String
type TTL = Int

type Payload = Int
type Rate = Double

data Direction
    = Forward
    | Backward
    deriving (Eq, Show)

data Destination
    = Unicast Ip Port
    | Multicast Ip Port Ip
    deriving (Eq, Show)

data CmdOptions = CmdOptions
    { optDirection   :: Direction
    , optDestination :: Destination
    , optRate        :: Rate
    } deriving (Generic, Eq, Show)

options :: Parser CmdOptions
options = CmdOptions
    <$> (flag' Backward ( long "backward" <> help "assume backward replay")
        <|> pure Forward)
    <*> (parseUnicast <|> parseMulticast)
    <*> option auto (long "rate" <> value 1 <> showDefault
        <> help "check interval (seconds)")
  where
    parseUnicast = Unicast
        <$> strOption (long "unicast" <> help "Unicast Ip")
        <*> strOption (long "port" <> help "Port number")
    parseMulticast = Multicast
        <$> strOption (long "multicast" <> help "Multicast Ip")
        <*> strOption (long "port" <> help "Port number")
        <*> strOption (long "localif" <> help "Local interface Ip")

runCmd :: CmdOptions -> Prog -> Args -> Version -> IO ()
runCmd (CmdOptions direction addr rate) _pName _pArgs _version = do
    cntOk <- newTVarIO (0 :: Integer)
    cntErr <- newTVarIO (0 :: Integer)
    cntErrTotal <- newTVarIO (0 :: Integer)
    let operator = case direction of
            Forward  -> (+)
            Backward -> (-)
        report = forever $ do
            threadDelay $ round (1000*1000 * rate)
            (c1,c2,c3) <- atomically $ (,,)
                <$> swapTVar cntOk 0
                <*> swapTVar cntErr 0
                <*> readTVar cntErrTotal
            putStrLn $ "ok: " ++ show c1 ++ ", err: " ++ show c2
                ++ ", errTotal: " ++ show c3
    withAsync report $ \_ -> bracket acquire release $ \sock -> do
        let loop n = do
                x <- getFirst sock
                atomically $ case x == n of
                    True -> modifyTVar cntOk succ
                    False -> do
                        modifyTVar cntErr succ
                        modifyTVar cntErrTotal succ

                loop (x `operator` 1)
        x <- getFirst sock
        atomically $ modifyTVar cntOk succ
        loop (x `operator` 1)
  where
    getFirst sock = fix $ \loop -> do
        msg <- fst <$> NB.recvFrom sock (2^(16::Int))
        case BS.length msg > 1 of
            False -> loop
            True  -> pure $ BS.head msg

    acquire = do
        let (ip, port, mc) = case addr of
                Unicast ip' port'         -> (ip', port', Nothing)
                Multicast mcast' port' m' -> (mcast', port', Just m')
        (serveraddr:_) <- Net.getAddrInfo
            (Just (Net.defaultHints {Net.addrFlags = [Net.AI_PASSIVE]}))
            (Just ip)
            (Just port)
        sock <- Net.socket
            (Net.addrFamily serveraddr) Net.Datagram Net.defaultProtocol
        case mc of
            Nothing -> pure ()
            Just mloc -> do
                Net.setSocketOption sock Net.ReuseAddr 1
                Mcast.addMembership sock ip (Just mloc)
        Net.bind sock (Net.addrAddress serveraddr)
        pure sock

    release = Net.close

-- | toplevel command
cmdTestReceiver :: ParserInfo Command
cmdTestReceiver = info
    ((runCmd <$> options) <**> helper)
    (progDesc "Test UDP receiver")
