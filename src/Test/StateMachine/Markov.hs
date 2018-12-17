{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.StateMachine.Markov
  ( Markov(..)
  , runMarkov
  , MarkovTable(..)
  , Continue(..)
  , tabulate
  , checkStochastic
  , gatherTransitions
  , markovToDot
  )
  where

import           Data.GraphViz
                   (graphElemsToDot, quickParams)
import           Data.GraphViz.Commands
                   (GraphvizOutput(Pdf), addExtension, runGraphviz)
import           Data.GraphViz.Exception
                   (GraphvizException, handle)
import           Generic.Data
                   (FiniteEnum, GBounded, GEnum, gfiniteEnumFromTo,
                   gmaxBound, gminBound)
import           GHC.Generics
                   (Generic1, Rep1, from1)
import           GHC.Generics
                   (Generic, Rep)
import           Prelude
import           System.Random
                   (RandomGen, mkStdGen, randomR)
import           Test.QuickCheck.Gen
                   (Gen, chooseAny)

import           Test.StateMachine.ConstructorName
                   (GConName1, gconName1)
import           Test.StateMachine.Types.History
                   (History, makeOperations, operationCommand,
                   operationIsCrash, unHistory)
import           Test.StateMachine.Types.References
                   (Symbolic)

------------------------------------------------------------------------

type ConstructorName = String

data Continue model state cmd
  = Stop
  | Continue ConstructorName (model Symbolic -> Gen (cmd Symbolic)) state

data Markov model state cmd = Markov
  { unMarkov :: state -> [(Int, Continue model state cmd)] }

newtype MarkovTable model state cmd = MarkovTable
  { unMarkovTable :: [(state, [(Int, Continue model state cmd)])] }

tabulate :: (Generic s, GEnum FiniteEnum (Rep s), GBounded (Rep s))
         => Markov m s a -> MarkovTable m s a
tabulate (Markov f) = MarkovTable
  [ (s, f s) | s <- gfiniteEnumFromTo gminBound gmaxBound ]

checkStochastic :: MarkovTable model state cmd -> Bool
checkStochastic = all (\is -> all (>= 0) is && sum is == 100)
                . filter (not . null)
                . map (map fst)
                . map snd
                . unMarkovTable

runMarkov :: forall model state cmd. Generic state
          => (GEnum FiniteEnum (Rep state), GBounded (Rep state))
          => Markov model state cmd -> model Symbolic -> state
          -> Gen (Maybe (cmd Symbolic, state))
runMarkov markov m s
  | checkStochastic (tabulate markov) = pickGen (unMarkov markov s)
  | otherwise                         = error "The probabilities don't add up to 100."
  where
    pickGen :: [(Int, Continue model state cmd)]
            -> Gen (Maybe (cmd Symbolic, state))
    pickGen gens = do
      stdGen <- mkStdGen <$> chooseAny
      frequencyR [ (prob, go gen) | (prob, gen) <- gens ] stdGen
      where
        go :: Continue model state cmd -> Gen (Maybe (cmd Symbolic, state))
        go Stop               = return Nothing
        go (Continue _c k s') = fmap (\x -> Just (x, s')) (k m)

    frequencyR :: RandomGen g => [(Int, b)] -> g -> b
    frequencyR []  _ = error "There are paths in the Markov chain which contain no generators."
    frequencyR xs0 g =
      let
        (n, _g') = randomR (1, 100) g
      in
        lookupRange n (makeRanges xs0)
      where
        makeRanges :: [(Int, b)] -> [((Int, Int), b)]
        makeRanges = go 1
          where
            go _   []               = []
            go low [(_, x)]         = [((low, 100), x)]
            go low ((high, x) : xs) = ((low, low + high), x) : go (low + high) xs

        lookupRange :: Int -> [((Int, Int), b)] -> b
        lookupRange needle = go
          where
            go [] = error "lookupRange: number not in any range"
            go (((low, high), x) : xs)
              | low <= needle && needle <= high = x
              | otherwise                       = go xs

data State state
  = NormalState state
  | StopState
  | CrashState

gatherTransitions :: (Generic1 cmd, GConName1 (Rep1 cmd))
                  => Markov model state cmd -> state -> History cmd resp
                  -> [(state, ConstructorName, State state)]
gatherTransitions markov start = go start [] . makeOperations . unHistory
  where
    go _state acc []         = reverse acc
    go state  acc (op : ops) =
      let
        cmd     = operationCommand op
        conName = gconName1 (from1 cmd)
        estate' = lookupMarkov markov state conName
      in
        if operationIsCrash op
        then reverse ((state, conName, CrashState) : acc)
        else case estate' of
          Nothing      -> error ("gatherTransitions: " ++ conName ++ " not found in Markov chain.")
          Just mstate' -> case (mstate', ops) of
            (StopState,          [])    -> reverse   ((state, conName, StopState)          : acc)
            (NormalState state', _ : _) -> go state' ((state, conName, NormalState state') : acc) ops
            (_, _)                      -> error "gatherTransitions: impossible."

lookupMarkov :: Markov model state cmd -> state -> ConstructorName
             -> Maybe (State state)
lookupMarkov (Markov markov) state conName = go (map snd (markov state))
  where
    go [] = Nothing
    go (Continue conName' _gen state' : cs)
      | conName == conName' = Just (NormalState state')
      | otherwise           = go cs
    go (Stop : _cs) = Just StopState

------------------------------------------------------------------------

markovToDot :: (Show state, Generic state)
            => (GEnum FiniteEnum (Rep state), GBounded (Rep state))
            => Markov model state cmd -> state -> FilePath -> IO (Either String FilePath)
markovToDot markov start fp =
  handle (\(e :: GraphvizException) -> return (Left (show e))) $ do
    let gr :: ([(String, String)], [(String, String, ConstructorName)])
        gr = markovToGraphElems markov start
    Right <$> addExtension (runGraphviz (uncurry (graphElemsToDot quickParams) gr)) Pdf fp

markovToGraphElems :: (Show state, Generic state)
                   => (GEnum FiniteEnum (Rep state), GBounded (Rep state))
                   => Markov model state cmd -> state
                   -> ([(String, String)], [(String, String, ConstructorName)])
markovToGraphElems markov _start = (nodes, edges)
  where
    table = tabulate markov
    exit  = "Exit"
    nodes = (exit, exit)
          : map (\e -> let s = show (fst e) in (s, s)) (unMarkovTable table)
    edges = concatMap (\(s, es) ->
                          map (\(freq, cont) -> let (s', a) = f freq cont in (show s, s', a))
                              es)
                      (unMarkovTable table)
      where
        f freq Stop                   = (exit, " Exit\n(" ++ show freq ++ "%)")
        f freq (Continue con _gen s') = (show s', ' ' : con ++ "\n(" ++ show freq ++ "%)")
