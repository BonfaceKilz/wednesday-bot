module Main where

import qualified Network.Socket as N
import           System.IO
import           System.Exit
import           Data.List

-- Config Options
myServer = "irc.freenode.org" :: String
myPort = 6667 :: N.PortNumber
myChan = "#urbanperspective"
myNick = "wednesday-dude-bot-test"

-- Connect to a server given its home and port number
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
  addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
  sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
  N.connect sock (N.addrAddress addr)
  N.socketToHandle sock ReadWriteMode

-- Send a message to a handle
write :: Handle -> String -> String -> IO ()
write h cmd args = do
  let msg = cmd ++ " " ++ args ++ "\r\n"
  hPutStr h msg
  putStr (">" ++ msg)

--- Process each line from the server
listen :: Handle -> IO ()
listen h = forever $ do
  line <- hGetLine h
  putStrLn line
  let s = init line
  if isPing s then pong s else eval h (clean s)
  where
    forever :: IO () -> IO ()
    forever a = do a; forever a

    clean :: String -> String
    clean = drop 1 . dropWhile (/= ':') . drop 1

    isPing :: String -> Bool
    isPing x = "PING :" `isPrefixOf` x

    pong :: String -> IO ()
    pong x = write h "PONG" (':' : drop 6 x)

eval :: Handle -> String -> IO ()
eval h "!quit"                   = write h "QUIT" ":Exiting" >> exitSuccess
eval h x | "!id " `isPrefixOf` x = privmsg h (drop 4 x)
eval _ _                         = return ()

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (myChan ++ " :" ++ s)

main :: IO ()
main = do
  h <- connectTo myServer myPort
  write h "NICK" myNick
  write h "USER" (myNick ++ " 0 * :tutorial bot")
  write h "JOIN" myChan
  listen h
