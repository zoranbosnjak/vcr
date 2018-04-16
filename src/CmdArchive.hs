------------------
-- |
-- Module: CmdArchive
--
-- TODO: WRITE COMPREHENSIVE DOCUMENTATION OF THE ARCHIVER


module CmdArchive (cmdArchive) where

-- Standard imports.
--import qualified Control.Exception    as CE
--import           Control.Monad           (forM_)
import           Control.Monad.IO.Class (liftIO)
{-
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
-}
--import           Network.HTTP.Client  as NC
--import qualified Network.HTTP.Types.Method
import           Options.Applicative     ((<**>), (<|>))
import qualified Options.Applicative  as Opt
--import           System.IO
import           System.Log.Logger       (Priority(INFO, DEBUG, NOTICE))
-- import Control.Exception as CE
-- import Data.Time (UTCTime(UTCTime))
-- import Network.HTTP.Client.Conduit
-- import qualified Data.Maybe as Maybe

-- Local imports.
import qualified Common as C
import qualified Event
import qualified Server as Srv
import qualified File
import qualified Encodings
import           Streams
-- import Test.QuickCheck hiding (output)

-- import Control.Concurrent

-- | The exported function implementing the entire functionality of
-- the archiver.  For more information see the documentation at the
-- top of this source file.
cmdArchive :: Opt.ParserInfo (C.VcrOptions -> IO ())
cmdArchive = Opt.info ((runCmd <$> options) <**> Opt.helper)
    (Opt.progDesc "Event archiver")


-- | Archiver specific command options.
data Options = Options
    { optInput          :: Input
    , optOutput         :: Output
    -- TODO: , optChannelFilter  :: [Event.Channel]
    -- TODO: , optSourceIdFilter :: [Event.SourceId]
    -- TODO: , optStartTime      :: Maybe UTCTime
    -- TODO: , optStopTime       :: Maybe UTCTime
    -- TODO: , read/write batch size and speed
    } deriving (Eq, Show)

-- | Input options.
data Input
    = IFile Encodings.EncodeFormat File.FileStore
    | IServer Srv.ServerConnection
    deriving (Eq, Show)

-- | Output options.
data Output
    = OFile Encodings.EncodeFormat File.FileStore
    | OServer Srv.ServerConnection
    deriving (Eq, Show)

-- | Command option parser.
options :: Opt.Parser Options
options = Options
    <$> input
    <*> output
    -- <*> Opt.many Event.channelOptions
    -- <*> Opt.many Event.sourceIdOptions
    -- <*> Opt.optional (C.timeOptions "start")
    -- <*> Opt.optional (C.timeOptions "stop")
    -- some more options...

input :: Opt.Parser Input
input = C.subparserCmd "input ..." $ Opt.command "input" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Data source")
  where
    opts = file <|> server
    file = Opt.flag' () (Opt.long "file") *>
        (IFile <$> Encodings.encodeFormatOptions <*> File.fileStoreOptions)
    server = Opt.flag' () (Opt.long "server")
        *> (IServer <$> Srv.serverConnectionOptions)

output :: Opt.Parser Output
output = C.subparserCmd "output ..." $ Opt.command "output" $ Opt.info
    (opts <**> Opt.helper)
    (Opt.progDesc "Data destination")
  where
    opts = file <|> server
    file = Opt.flag' () (Opt.long "file") *>
        (OFile <$> Encodings.encodeFormatOptions <*> File.fileStoreOptions)
    server = Opt.flag' () (Opt.long "server")
        *> (OServer <$> Srv.serverConnectionOptions)



-- | Run command.
runCmd :: Options -> C.VcrOptions -> IO ()
runCmd opts vcrOpts = do
    C.logM INFO $
        "archive: " ++ show opts ++ ", vcrOpts: " ++ show vcrOpts

    runStream $ source >-> trace >-> Streams.filter flt >-> target

    C.logM INFO $
        "archive: done"

  where

    -- get events
    source = case optInput opts of
        -- TODO:
        --  - configurable chunk size with default value
        --  - configurable max item size
        IFile inpEnc inpFS ->
            File.fileReaderChunks 32752 inpFS
            >-> Encodings.fromByteString (100*1024) inpEnc

        IServer _inpSC -> undefined

    -- log errors and progress
    trace = mkPipe $ \consume produce -> forever $ do
        result <- consume Clear
        case result of
            Left e -> do
                liftIO $ C.logM NOTICE $ "Cannot decode an event." ++ show e
            Right event -> do
                liftIO $ C.logM DEBUG $
                    "archive: event " ++ show (Event.eUtcTime event)
                produce event

    -- TODO: implement filter function: Event -> Bool,
    -- based on given arguments
    flt _event = True

    -- store events
    target = case optOutput opts of
        OFile outEnc outFS ->
            Encodings.toByteString outEnc
            >-> File.fileWriter outFS Nothing
            >-> drain -- ignore rotate information
        OServer _outSC -> undefined

{-
-- | Copies one file of events to another file of events, possibly in
-- a different format.
-- Throws an exception
-- (1) if any IO operation on files fails, or
-- (2) if any segment of the input file cannot be recognized as an event.
copyFromFileToFile ::
    (Encodings.EncodeFormat,File.FileStore)
    -> (Encodings.EncodeFormat,File.FileStore)
    -> IO ()
copyFromFileToFile (inpEnc,inpFS) (outEnc,outFS) = do
    C.logM DEBUG $ "archive: file to file started"

    -- Open the input and the output file:
    inpFH <- if (File.filePath inpFS)=="-" then return stdin else
                 CE.catch (openFile (File.filePath inpFS) ReadMode)
                   ((\ _ -> do
                         C.throw ("archive: Cannot open input file '"++
                                  (File.filePath inpFS)++"'."))
                    :: (CE.IOException -> IO Handle))
    outFH <- if (File.filePath outFS)=="-" then return stdout else
                 CE.catch (openFile ((File.filePath outFS)++"") WriteMode)
                   ((\ _ -> do
                         hClose inpFH
                         C.throw ("archive: Cannot open output file '"++
                                  (File.filePath outFS)++"'."))
                    :: (CE.IOException -> IO Handle))

    -- Get the complete input string and split it to bytestrings
    -- representing individual events using the separator defined by
    -- the input encoding:
    bss <- split inpEnc <$> BS.hGetContents inpFH

    -- Decode each bytestring representing an individual event write
    -- the event to the output file one by one.
    -- TODO: check performance, take more events at the same time.
    forM_ bss $ \ bs -> case Encodings.decode inpEnc bs of
        Nothing -> do
            hClose inpFH
            hClose outFH
            C.throw "archive: Cannot decode an event."
        Just event -> do
            C.logM DEBUG $ "archive: event " ++ show (Event.eUtcTime event)
            BS.hPutStr outFH
              (Encodings.encode outEnc (event::Event.Event))

    -- Close the input and the output file:
    if (File.filePath inpFS)=="-" then return () else hClose inpFH
    if (File.filePath outFS)=="-" then return () else hClose outFH

    C.logM DEBUG $ "archive: file to file done"
    return ()



copyFromHTTPToFile ::
    Srv.ServerConnection
    -> (Encodings.EncodeFormat,File.FileStore)
    -> IO ()
copyFromHTTPToFile inpSC (outEnc,outFS) = do
    C.logM DEBUG $ "archive: http to file started"

    -- Open the output file:
    outFH <- if (File.filePath outFS)=="-" then return stdout else
                 CE.catch (openFile ((File.filePath outFS)++"") WriteMode)
                   ((\ _ -> do
                         C.throw ("archive: Cannot open output file '"++
                                  (File.filePath outFS)++"'."))
                    :: (CE.IOException -> IO Handle))
    -- Prepare the input connection:
    manager <- NC.newManager
                   (NC.defaultManagerSettings {
                       managerResponseTimeout =
                           NC.responseTimeoutMicro
                               (round ((Srv.connectTimeout inpSC) * 1e6)) })

    let Srv.URI uri = head (Srv.serverPool inpSC)
    request <- parseRequest ("GET " ++ uri)
    C.logM DEBUG $ (show request)

    withResponse request manager $ \ response -> do
        let loop buffer = do
                -- Read the new data and make it lazy:
                newInpData8 <- NC.brRead $ NC.responseBody response
                let newInpDataL = {-BSL.fromStrict-} newInpData8    -- TODO: strict is used by default
                if BS.null newInpDataL then
                    do -- Try converting the buffer data:
                       let mbEvents =
                            (Encodings.decodeList maxBound Encodings.EncBin buffer)
                            ::(Maybe [Event.Event])
                       if Maybe.isJust mbEvents then
                           do let newOutDataL =
                                      Encodings.encodeList outEnc
                                          (Maybe.fromJust mbEvents)
                              BS.hPut outFH newOutDataL
                              return ()
                       else
                           do return ()
                else
                    do -- Append the new data to the buffer:
                       putStr ((show (BS.length newInpDataL))++":")
                       let newBuffer = BS.append buffer newInpDataL
                       putStr ((show (BS.length newBuffer))++" ")
                       loop newBuffer
                {-
                if BSL.null newInpDataL then return () else do
                    -- Append the new data to the buffer:
                    putStr ((show (BSL.length newInpDataL))++":")
                    let newBuffer = BSL.append buffer newInpDataL
                    putStr ((show (BSL.length newBuffer))++" ")
                    -- Try converting the buffer data:
                    let mbEvents =
                            (Encodings.decodeList Encodings.EncBin newBuffer)
                            ::(Maybe [Event.Event])
                    if False -- Maybe.isJust mbEvents
                      then do putStr ("Just ")
                              let newOutDataL =
                                      Encodings.encodeList outEnc
                                          (Maybe.fromJust mbEvents)
                              BSL.hPut outFH newOutDataL
                              loop BSL.empty
                      else loop newBuffer
                -}
        loop BS.empty

    -- Close the output file:
    if (File.filePath outFS)=="-" then return () else hClose outFH

    C.logM DEBUG $ "archive: http to file done"
    return ()



copyFromFileToHTTP_ :: (Encodings.EncodeFormat,File.FileStore) -> Srv.ServerConnection -> IO ()
copyFromFileToHTTP_ (inpEnc,inpFS) outSC = do
    C.logM DEBUG $ "archive: file to http started"

    -- Open the input file:
    inpFH <- if (File.filePath inpFS)=="-" then return stdin else
                 CE.catch (openFile (File.filePath inpFS) ReadMode)
                   ((\ _ -> do
                         C.throw ("archive: Cannot open input file '"++
                                  (File.filePath inpFS)++"'."))
                    :: (CE.IOException -> IO Handle))
    -- Prepare the server connection:
    manager <- NC.newManager (NC.defaultManagerSettings { managerResponseTimeout = NC.responseTimeoutMicro 1000000 })

    outFH <- openFile "bla.bin" WriteMode

    -- Get the complete input string and split it to bytestrings
    -- representing individual events using the separator defined by
    -- the input encoding:
    bss <- split inpEnc <$> BS.hGetContents inpFH

    -- Decode each bytestring representing an individual event write
    -- the event to the output file one by one.
    -- TODO: check performance, take more events at the same time.
    forM_ (take 1 bss) $ \ bs -> case Encodings.decode inpEnc bs of
        Nothing -> do
            hClose inpFH
            C.throw "archive: Cannot decode an event."
        Just event -> do
            C.logM DEBUG $ "archive: event " ++ show (Event.eUtcTime event)

            --create event as a lazy bytestring and put it into a request

            request <- return (NC.defaultRequest {
                           method = Network.HTTP.Types.Method.methodGet,
                           host = BS8.pack "sliva.fri.uni-lj.si",
                           port = 80,
                           path = BS8.pack "~sliva/ubuntu.iso"})
            C.logM DEBUG $ (show request)
            {-
            request <- return (NC.defaultRequest {
                           method = Network.HTTP.Types.Method.methodGet,
                           host = BS8.pack "www.brainjar.com",
                           port = 80,
                           path = BS8.pack "java/host/test.html"})
            C.logM DEBUG $ (show request)
            -}
            {-
            let request = NC.setRequestMethod "GET"
                        $ request'
                           --method = Network.HTTP.Types.Method.methodGet,
                           --host = BSL.pack "www.brainjar.com"}) -}
            -- response <- NC.httpLbs request manager

            withResponse request manager $ \ response -> do
              putStrLn $ "Status: "++(show (NC.responseStatus response))
              let loop = do
                    bs <- NC.brRead $ NC.responseBody response
                    if BS8.null bs
                      then do putStrLn ""
                              return ()
                      else do putStr "."
                              BS8.hPutStr outFH bs
                              threadDelay 500
                              loop
              loop

            --putStrLn $ "Body: "++(show (NC.responseBody response))

            --BSL.hPutStr outFH
            --  (Encodings.encode outEnc (event::Event.Event))
            return ()

    hClose outFH

    -- Close the input file:
    if (File.filePath inpFS)=="-" then return () else hClose inpFH

    C.logM DEBUG $ "archive: file to http done"
    return ()

-- | This function was removed from Encodings module... (deprecated).
-- Please rewrite program to use Encodings.fromByteString
-- or Encodings.decodeStream functions instead.
-- Here is a dummy version of the split function, just to make program work,
-- but this function shall not be used otherwise.
split :: Encodings.EncodeFormat -> BS.ByteString -> [BS.ByteString]
split fmt s = case sequence (check <$> result) of
    -- in case of problems just return empty list
    Nothing -> []
    -- On success, encode each Event, so that it can be decoded
    -- again later (this is the dummy part).
    Just lst -> Encodings.encode fmt <$> (lst :: [Event.Event])
  where
    check (Left _) = Nothing
    check (Right a) = Just a
    -- feed input string to the parsing pipe and collect results to the list.
    result = PP.toList (Pipes.yield s >-> Encodings.fromByteString maxBound fmt)
-}

