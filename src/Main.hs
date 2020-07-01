module Main where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.List
import           System.IO
import           System.Exit
import qualified Network.Socket as N
import           Control.Monad.Trans.Reader

-- Config Options
myServer = "irc.freenode.org" :: String
myPort = 6667 :: N.PortNumber
myChan = "#urbanperspective" :: String
myNick = "wednesday-dude-bot-test" :: String

-- Main program entry
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . botSocket
    loop st    = runReaderT run st

-- Net monad
data Bot = Bot { botSocket :: Handle }
type Net = ReaderT Bot IO

-- Connect to a server given its home and port number
connect :: IO Bot
connect = notify $ do
  h <- connectTo myServer myPort
  return (Bot h)
  where
    notify a = bracket_
      (putStrLn ("Connecting to " ++ myServer ++ " ...") >> hFlush stdout)
      (putStrLn "done.")
      a

-- Connect to the server and return a handle
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
  addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
  sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
  N.connect sock (N.addrAddress addr)
  N.socketToHandle sock ReadWriteMode

run :: Net ()
run = do
  write "NICK" myNick
  write "USER" (myNick ++ " 0 * : tutorial bot")
  write "JOIN" myChan
  listen
  
-- Send a message to a handle
write :: String -> String -> Net ()
write cmd args = do
  h <- asks botSocket
  let msg = cmd ++ " " ++ args ++ "\r\n"
  liftIO $ hPutStr h msg
  liftIO $ putStr ("> " ++ msg)

--- Process each line from the server
listen :: Net ()
listen = forever $ do
  h <- asks botSocket
  line <- liftIO $ hGetLine h
  liftIO (putStrLn line)
  let s = init line
  if isPing s then pong s else eval (clean s)
  where
    forever :: Net () -> Net ()
    forever a = do a; forever a

    clean :: String -> String
    clean = drop 1 . dropWhile (/= ':') . drop 1

    isPing :: String -> Bool
    isPing x = "PING :" `isPrefixOf` x

    pong :: String -> Net ()
    pong x = write "PONG" (':' : drop 6 x)

eval :: String -> Net ()
eval "!quit"                   = write "QUIT" ":Exiting" >> liftIO exitSuccess
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval _                         = return ()

privmsg :: String -> Net ()
privmsg msg = write "PRIVMSG" (myChan ++ " :" ++ msg)
