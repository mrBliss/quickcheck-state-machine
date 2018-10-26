{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Concurrent
                   (forkFinally)
import           Control.Exception
import           Control.Monad
                   (forever, unless, void)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as Char8
import           Network.Socket            hiding
                   (recv)
import           Network.Socket.ByteString
                   (recv, sendAll)

------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ do
  addr <- resolve "33000"
  bracket (open addr) close loop
  where
    resolve port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 10
        return sock
    loop sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        void $ forkFinally (talk conn) (\_ -> close conn)
    talk conn = do
        msg <- (recv conn 1024)
                  -- `catch` (\(err :: IOError) -> print err >> return Nothing)
        unless (BS.null msg) $ do
          Char8.putStrLn ("Received: " <> msg)
          talk conn
