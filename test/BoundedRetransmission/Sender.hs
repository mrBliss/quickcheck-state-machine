{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Concurrent
                   (forkFinally)
import           Control.Exception
import           Control.Monad
                   (forever, unless, void)
import qualified Data.ByteString.Char8     as BS
import           Network.Socket            hiding
                   (recv)
import           Network.Socket.ByteString
                   (recv, sendAll)
import           Prelude

------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ do
  addr <- resolve "127.0.0.1" "33000"
  bracket (open addr) close talk
  where
    resolve host port = do
      let hints = defaultHints { addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock (addrAddress addr)
      return sock
    talk sock = forever $ do
      s <- BS.getLine
      sendAll sock s
        `catch` (\(err :: IOError) -> print err >> talk sock)
      BS.putStrLn ("Sent: " <> s)
