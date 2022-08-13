import Text.Printf
import Network.Socket
import Network.Socket.ByteString (send, recv)
import Data.ByteString.Char8 (pack)
import Control.Applicative
import Control.Exception (try, catch, SomeException)
import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent
import System.Environment
import System.IO

-- data representing the actual state
data ServerState' = ServerState
                  { connections :: Int }

-- The actual state wrapped in TVar to allow concurrent access
type ServerState = TVar ServerState'

-- Our monad.
type Handler a = StateT ServerState IO a

-- | The programs entry point. Starts the TCP server and kicks off
-- infinite loop to accept and handle the connections.
main = do
    port <- (\xs -> if length xs > 0 then xs!!0 else "8888") <$> getArgs
    addrinfos <- getAddrInfo
        (Just defaultHints {addrFamily = AF_INET})
        (Just "127.0.0.1")
        (Just port)

    let ai = head addrinfos
    print ai

    ss <- atomically . newTVar $ ServerState 0

    sock <- socket (addrFamily ai) Stream defaultProtocol
    _ <- bind sock $ addrAddress ai
    _ <- listen sock 5

    forever $ accept sock >>= forkIO . handler ss

-- | Connection handler.
handler :: ServerState -> (Socket, SockAddr) -> IO ()
handler ss c = evalStateT (handle c >> close' c) ss
  where
    -- | Handle the connection inside our Handler monad, hence have
    -- access to the state via the TVar given by `get`
    handle :: (Socket, SockAddr) -> Handler ()
    handle (c, addr) = do
        -- To demonstrate the server is indeed forking, print a hello
        -- and wait for a line of input.
        -- liftIO . send c . printf "Hello: %s\n" $ show addr
        _ <- liftIO $ recv c 5

        -- get the TVar holding our server's state
        ts <- get

        -- atomically read the current connection number update the
        -- state with bumped one
        connNo <- liftIO . atomically $ do
            s <- readTVar ts
            let x = connections s
            writeTVar ts $ s { connections = succ x }
            return x

        -- Write the state to the client.
        void . liftIO . send c $ pack $ printf "You are: %d.\n" connNo

    close' (c , _) = liftIO
        $ socketToHandle c ReadWriteMode
            >>= \h -> hFlush h >> hClose h
