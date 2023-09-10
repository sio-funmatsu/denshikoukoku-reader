import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.ICU.Convert as ICU
import Network.Socket (
    AddrInfo (addrAddress, addrFamily, addrProtocol, addrSocketType),
    HostName,
    PortNumber,
    Socket,
    SocketType (Stream),
    close,
    connect,
    defaultHints,
    getAddrInfo,
    socket,
    socketToHandle,
 )
import System.IO (
    BufferMode (NoBuffering),
    Handle,
    IOMode (ReadWriteMode),
    hIsEOF,
    hSetBuffering,
    stdin,
    stdout,
 )

-- | Main function.
main :: IO ()
main = do
    (encoder, decoder) <- initializeConverters
    let host = "koukoku.shadan.open.ad.jp"
        port = 23
    addr <- resolve host port
    bracket (open addr) close (talk encoder decoder)

-- | Initialize converters.
initializeConverters :: IO (ICU.Converter, ICU.Converter)
initializeConverters = do
    enc <- ICU.open "Shift_JIS" Nothing
    dec <- ICU.open "UTF-8" Nothing
    return (enc, dec)

-- | Resolve host name and port number.
resolve :: HostName -> PortNumber -> IO AddrInfo
resolve host port = do
    let hints = defaultHints{addrSocketType = Stream}
    head <$> getAddrInfo (Just hints) (Just host) (Just (show port))

-- | Open socket.
open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    return sock

-- | Talk with server.
talk :: ICU.Converter -> ICU.Converter -> Socket -> IO ()
talk encoder decoder sock = do
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering
    _ <- forkIO $ copyShiftJIS decoder encoder handle stdout
    copyShiftJIS decoder encoder stdin handle

-- | Copy Shift_JIS data.
copyShiftJIS :: ICU.Converter -> ICU.Converter -> Handle -> Handle -> IO ()
copyShiftJIS encoder decoder src dst = do
    eof <- hIsEOF src
    unless eof $ do
        c <- BSC.hGetLine src
        let text = ICU.toUnicode decoder c
        let shiftJisData = ICU.fromUnicode encoder text
        BSC.hPutStrLn dst shiftJisData
        copyShiftJIS encoder decoder src dst
