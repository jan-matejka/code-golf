import System.Environment
import Text.Printf
import Network.Socket
import Control.Applicative
import Control.Exception (try, catch, SomeException)
import Control.Monad.State
import System.IO
import Control.Concurrent.Thread.Delay

-- | State of our stateful monad - Handler
data ServerState = ServerState
                 { connections :: Int }

-- | Our stateful monad. Created by a monad transformer StateT
-- by using ServerState as state,
-- wrapping around IO,
-- returning value `a`.
type Handler a = StateT ServerState IO a

get_port (x:xs) = x
get_port [] = "8888"

-- | The programs entry point. Starts the TCP server and kicks off
-- infinite loop to accept and handle the connections.
main = do
    args <- getArgs

    addrinfos <- getAddrInfo
        (Just defaultHints {addrFamily = AF_INET})
        (Just "127.0.0.1")
        (Just $ get_port args)

    let ai = head addrinfos
        ss = ServerState 0
    print ai

    sock <- socket (addrFamily ai) Stream defaultProtocol
    _ <- bind sock $ addrAddress ai
    _ <- listen sock 5

    ouroboros ((accept sock >>=) . handler) ss

-- | Helper function to run connection handler with initial state
-- and feed the resulting new state into the next handler
ouroboros :: (a -> IO a) -> a -> IO ()
ouroboros fx old = fx old >>= ouroboros fx

-- | Connection handler. Takes current ServerState, the connection and
-- returns resulting new ServerState
handler :: ServerState -> (Socket, SockAddr) -> IO ServerState
handler ss (c, _) = snd <$> runStateT (handle c >> close' c) ss
  where
    -- | Handle the connection inside our Handler monad, hence have
    -- access to the state via the `get` and `put` functions
    handle :: Socket -> Handler ()
    handle c = do
        -- get the current state
        connNo <- connections <$> get

        -- Write the state to the client.
        -- As the send functions is IO () and not Handler a, it needs to
        -- be lifted into the inner IO monad
        liftIO $ send c $ printf "You are: %d.\n" connNo

        -- Write the new state
        put $ ss { connections = succ connNo }

        -- liftIO $ delay 5000

    close' c = liftIO
        $ socketToHandle c ReadWriteMode
            >>= \h -> hFlush h >> hClose h
