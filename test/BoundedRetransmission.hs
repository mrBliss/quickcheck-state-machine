{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RecordWildCards #-}

module BoundedRetransmission (main) where

import           Control.Concurrent
                   (threadDelay)
import           Prelude
import           System.IO
                   (Handle, hFlush, hPutStrLn)
import           System.Process
                   (ProcessHandle, StdStream(CreatePipe), callCommand,
                   createProcess, getPid, proc, spawnProcess, std_in,
                   terminateProcess)

import           Test.StateMachine

------------------------------------------------------------------------

data Action (r :: * -> *)
  = SpawnReceiver
  | SpawnSender
  | Send Handle Char
  | WhatHasBeenTransfered
  | BreakConnection (Reference ProcessHandle r)
  | FixConnection (Reference ProcessHandle r)

data Response (r :: * -> *)
  = SpawnedReceiver (Reference ProcessHandle r)
  | SpawnedSender (Reference ProcessHandle r) (Reference Handle r)
  | Sent
  | ThisHasBeenTransfered String
  | BrokeConnection
  | FixedConnection

data Model (r :: * -> *)
  = Init
  | ReceiverReady (Reference ProcessHandle r)
  | Running
      { sender   :: Reference ProcessHandle r
      , handle   :: Handle
      , receiver :: Reference ProcessHandle r
      , message  :: String
      , faulty   :: Bool
      }

transition :: Model r -> Action r -> Response r -> Model r
transition Init SpawnReceiver (SpawnedReceiver pid) =
  ReceiverReady pid
transition Running {..} (Send _h c) Sent
  | faulty    = Running {..}
  | otherwise = Running { message = message ++ [c], .. }

semantics :: Action Concrete -> IO (Response Concrete)
semantics (Send h c) = do
  hPutStrLn h [c]
  hFlush h
  return Sent
semantics (BreakConnection ph) = do
  Just pid <- getPid (concrete ph)
  callCommand ("fiu-ctrl -c \"disable name=posix/io/net/send\" " ++ show pid)
  return BrokeConnection

------------------------------------------------------------------------

main :: IO ()
main = do
  rph <- spawnProcess "fiu-run" ["-x", "stack", "exec", "receiver"]
  Just rpid <- getPid rph
  print rpid
  threadDelay 1000000
  -- callCommand ("fiu-ctrl -c \"enable name=posix/io/net/recv\" " ++ show rpid)
  -- _ <- spawnProcess "stack" ["exec", "sender"]
  (Just hin, _, _, sph) <- createProcess
    (proc "stack" ["exec", "sender"]){ std_in = CreatePipe }
  threadDelay 1000000
  hPutStrLn hin "a"
  hFlush hin
  -- callCommand ("fiu-ctrl -c \"disable name=posix/io/net/recv\" " ++ show rpid)
  -- callProcess "stack" ["exec", "sender"]
  threadDelay 5000000
  terminateProcess rph
  terminateProcess sph

