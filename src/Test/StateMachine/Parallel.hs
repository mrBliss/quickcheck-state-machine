{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Test.StateMachine.Parallel
-- Copyright   :  (C) 2017, ATS Advanced Telematic Systems GmbH
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Stevan Andjelkovic <stevan.andjelkovic@here.com>
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- This module contains helpers for generating, shrinking, and checking
-- parallel programs.
--
-----------------------------------------------------------------------------

module Test.StateMachine.Parallel
  ( forAllParallelCommands
  , generateParallelCommands
  , shrinkParallelCommands
  , validParallelCommands
  , prop_splitCombine
  , runParallelCommands
  , runParallelCommandsNTimes
  , executeParallelCommands
  , linearise
  , toBoxDrawings
  , prettyParallelCommands
  ) where

import           Control.Arrow
                   ((***))
import           Control.Concurrent.Async.Lifted
                   (concurrently)
import           Control.Concurrent.STM.TChan
                   (newTChanIO)
import           Control.Monad
                   (foldM, replicateM)
import           Control.Monad.Catch
                   (MonadCatch)
import           Control.Monad.State.Strict
                   (MonadIO, State, evalState, put, runStateT)
import           Control.Monad.Trans.Control
                   (MonadBaseControl, liftBaseWith)
import           Data.Bifunctor
                   (bimap)
import           Data.List
                   (partition, permutations)
import           Data.List.Split
                   (splitPlacesBlanks)
import           Data.Monoid
                   ((<>))
import           Data.Set
                   (Set)
import qualified Data.Set                        as S
import           Data.Tree
                   (Tree(Node))
import           Generic.Data
                   (FiniteEnum, GBounded, GEnum)
import           GHC.Generics
                   (Generic, Rep)
import           Prelude
import           Test.QuickCheck
                   (Gen, Property, Testable, choose, property,
                   shrinkList, sized)
import           Test.QuickCheck.Monadic
                   (PropertyM, run)
import           Text.PrettyPrint.ANSI.Leijen
                   (Doc)
import           Text.Show.Pretty
                   (ppShow)

import           Test.StateMachine.BoxDrawer
import           Test.StateMachine.Logic
import           Test.StateMachine.Sequential
import           Test.StateMachine.Types
import qualified Test.StateMachine.Types.Rank2   as Rank2
import           Test.StateMachine.Utils

------------------------------------------------------------------------

forAllParallelCommands :: Testable prop
                       => (Show (model Symbolic), Show submodel, Show (cmd Symbolic))
                       => (Generic submodel, GEnum FiniteEnum (Rep submodel), GBounded (Rep submodel))
                       => (Rank2.Foldable cmd, Rank2.Foldable resp)
                       => AdvancedStateMachine model submodel cmd m resp
                       -> (ParallelCommands cmd -> prop)     -- ^ Predicate.
                       -> Property
forAllParallelCommands sm =
  forAllShrinkShow (generateParallelCommands sm) (shrinkParallelCommands sm) ppShow

generateParallelCommands :: forall model submodel cmd m resp. Rank2.Foldable resp
                         => (Show (model Symbolic), Show submodel, Show (cmd Symbolic))
                         => (Generic submodel, GEnum FiniteEnum (Rep submodel), GBounded (Rep submodel))
                         => AdvancedStateMachine model submodel cmd m resp
                         -> Gen (ParallelCommands cmd)
generateParallelCommands sm@StateMachine { initModel } = do
  Commands cmds      <- generateCommands sm Nothing
  prefixLength       <- sized (\k -> choose (0, k `div` 3))
  let (prefix, rest) =  bimap Commands Commands (splitAt prefixLength cmds)
  return (ParallelCommands prefix
            (makeSuffixes (advanceModel sm initModel newCounter prefix) rest))
  where
    makeSuffixes :: (model Symbolic, Counter) -> Commands cmd -> [Pair (Commands cmd)]
    makeSuffixes (model0, counter0) = go (model0, counter0) [] . unCommands
      where
        go _                acc []   = reverse acc
        go (model, counter) acc cmds = go (advanceModel sm model counter (Commands safe))
                                          (Pair (Commands safe1) (Commands safe2) : acc)
                                          rest
          where
            (safe, rest)   = spanSafe model counter [] cmds
            (safe1, safe2) = splitAt (length safe `div` 2) safe

        suffixLength = 5

        spanSafe :: model Symbolic -> Counter -> [Command cmd] -> [Command cmd]
                 -> ([Command cmd], [Command cmd])
        spanSafe _     _       safe []                         = (reverse safe, [])
        spanSafe model counter safe (cmd@(Command _ _) : cmds)
          | length safe <= suffixLength &&
              parallelSafe sm model counter (Commands (cmd : safe)) =
                spanSafe model counter (cmd : safe) cmds
          | otherwise = (reverse safe, cmd : cmds)

-- | A list of commands is parallel safe if the pre-conditions for all commands
--   hold in all permutations of the list.
parallelSafe :: AdvancedStateMachine model submodel cmd m resp -> model Symbolic
             -> Counter -> Commands cmd -> Bool
parallelSafe StateMachine { precondition, transition, mock } model0 counter0
  = and
  . map (preconditionsHold model0 counter0)
  . permutations
  . unCommands
  where
    preconditionsHold _     _       []                         = True
    preconditionsHold model counter (Command cmd _vars : cmds) =
      let
        (resp, counter') = runGenSym (mock model cmd) counter
      in
        boolean (precondition model cmd) &&
          preconditionsHold (transition model cmd resp) counter' cmds

-- | Apply the transition of some commands to a model.
advanceModel :: AdvancedStateMachine model submodel cmd m resp
             -> model Symbolic  -- ^ The model.
             -> Counter
             -> Commands cmd    -- ^ The commands.
             -> (model Symbolic, Counter)
advanceModel StateMachine { transition, mock } model0 counter0 =
  go model0 counter0 . unCommands
  where
    go model counter []                         = (model, counter)
    go model counter (Command cmd _vars : cmds) =
      let
        (resp, counter') = runGenSym (mock model cmd) counter
      in
        go (transition model cmd resp) counter' cmds

------------------------------------------------------------------------

-- | Shrink a parallel program in a pre-condition and scope respecting
--   way.
shrinkParallelCommands
  :: forall cmd model submodel m resp. Rank2.Foldable cmd
  => Rank2.Foldable resp
  => AdvancedStateMachine model submodel cmd m resp
  -> (ParallelCommands cmd -> [ParallelCommands cmd])
shrinkParallelCommands sm@StateMachine { shrinker, initModel }
                       (ParallelCommands prefix suffixes)
  = filterMaybe (flip evalState (initModel, S.empty, newCounter) . validParallelCommands sm)
      [ ParallelCommands prefix' (map toPair suffixes')
      | (prefix', suffixes') <- shrinkPair' shrinkCommands' shrinkSuffixes
                                            (prefix, map fromPair suffixes)
      ]
      ++
      shrinkMoveSuffixToPrefix
  where
    shrinkCommands' :: Commands cmd -> [Commands cmd]
    shrinkCommands'
      = map Commands
      . shrinkList (liftShrinkCommand shrinker)
      . unCommands

    shrinkSuffixes :: [(Commands cmd, Commands cmd)] -> [[(Commands cmd, Commands cmd)]]
    shrinkSuffixes = shrinkList (shrinkPair shrinkCommands')

    shrinkMoveSuffixToPrefix :: [ParallelCommands cmd]
    shrinkMoveSuffixToPrefix = case suffixes of
      []                   -> []
      (suffix : suffixes') ->
        [ ParallelCommands (prefix <> Commands [prefix'])
                           (fmap Commands (toPair suffix') : suffixes')
        | (prefix', suffix') <- pickOneReturnRest2 (unCommands (proj1 suffix),
                                                    unCommands (proj2 suffix))
        ]

    pickOneReturnRest :: [a] -> [(a, [a])]
    pickOneReturnRest []       = []
    pickOneReturnRest (x : xs) = (x, xs) : map (id *** (x :)) (pickOneReturnRest xs)

    pickOneReturnRest2 :: ([a], [a]) -> [(a, ([a],[a]))]
    pickOneReturnRest2 (xs, ys) =
      map (id *** flip (,) ys) (pickOneReturnRest xs) ++
      map (id ***      (,) xs) (pickOneReturnRest ys)

validParallelCommands :: forall model submodel cmd m resp.
                         (Rank2.Foldable cmd, Rank2.Foldable resp)
                      => AdvancedStateMachine model submodel cmd m resp
                      -> ParallelCommands cmd
                      -> State (model Symbolic, Set Var, Counter) (Maybe (ParallelCommands cmd))
validParallelCommands sm@StateMachine { initModel } (ParallelCommands prefix suffixes) = do
  let prefixLength       = lengthCommands prefix
      leftSuffixes       = map (\(Pair l _r) -> l) suffixes
      rightSuffixes      = map (\(Pair _l r) -> r) suffixes
      leftSuffixLengths  = map lengthCommands leftSuffixes
      rightSuffixLengths = map lengthCommands rightSuffixes
      leftSuffix         = mconcat leftSuffixes
      rightSuffix        = mconcat rightSuffixes
  mleft  <- validCommands sm (prefix <> leftSuffix)
  put (initModel, S.empty, newCounter)
  mright <- validCommands sm (prefix <> rightSuffix)
  case (mleft, mright) of
    (Nothing, Nothing)      -> return Nothing
    (Just _,  Nothing)      -> return Nothing
    (Nothing, Just _)       -> return Nothing
    (Just left, Just right) ->
      case (splitPlacesBlanks (prefixLength : leftSuffixLengths)  (unCommands left),
            splitPlacesBlanks (prefixLength : rightSuffixLengths) (unCommands right)) of
        ([]                     , [])                 -> error "validParallelCommands: impossible"
        ([]                     , _ : _)              -> error "validParallelCommands: impossible"
        (_ : _                  , [])                 -> error "validParallelCommands: impossible"
        (prefix' : leftSuffixes', _ : rightSuffixes') -> do
          let suffixes' = zipWith Pair (map Commands leftSuffixes')
                                       (map Commands rightSuffixes')
              (model', counter') = advanceModel sm initModel newCounter (Commands prefix')

          if parallelSafeMany sm model' counter' suffixes'
          then return (Just (ParallelCommands (Commands prefix') suffixes'))
          else return Nothing

parallelSafeMany :: AdvancedStateMachine model submodel cmd m resp -> model Symbolic
                 -> Counter -> [Pair (Commands cmd)] -> Bool
parallelSafeMany sm = go
  where
    go _ _ []                                   = True
    go model counter (Pair cmds1 cmds2 : cmdss) = parallelSafe sm model counter cmds
                                                && go model' counter' cmdss
      where
        cmds               = cmds1 <> cmds2
        (model', counter') = advanceModel sm model counter cmds

prop_splitCombine :: [[Int]] -> Bool
prop_splitCombine xs = splitPlacesBlanks (map length xs) (concat xs) == xs

------------------------------------------------------------------------

runParallelCommands :: (Show (cmd Concrete), Show (resp Concrete))
                    => (Rank2.Traversable cmd, Rank2.Foldable resp)
                    => (MonadCatch m, MonadBaseControl IO m)
                    => AdvancedStateMachine model submodel cmd m resp
                    -> ParallelCommands cmd
                    -> PropertyM m [(History cmd resp, Logic)]
runParallelCommands sm = runParallelCommandsNTimes 10 sm

runParallelCommandsNTimes :: (Show (cmd Concrete), Show (resp Concrete))
                          => (Rank2.Traversable cmd, Rank2.Foldable resp)
                          => (MonadCatch m, MonadBaseControl IO m)
                          => Int -- ^ How many times to execute the parallel program.
                          -> AdvancedStateMachine model submodel cmd m resp
                          -> ParallelCommands cmd
                          -> PropertyM m [(History cmd resp, Logic)]
runParallelCommandsNTimes n sm cmds =
  replicateM n $ do
    (hist, _reason) <- run (executeParallelCommands sm cmds)
    return (hist, linearise sm hist)

executeParallelCommands :: (Rank2.Traversable cmd, Rank2.Foldable resp)
                        => (MonadCatch m, MonadBaseControl IO m)
                        => AdvancedStateMachine model submodel cmd m resp
                        -> ParallelCommands cmd
                        -> m (History cmd resp, Reason)
executeParallelCommands sm@StateMachine{ initModel } (ParallelCommands prefix suffixes) = do

  hchan <- liftBaseWith (const newTChanIO)

  (reason0, (env0, _cmodel)) <- runStateT
    (executeCommands sm hchan (Pid 0) True prefix)
    (emptyEnvironment, initModel)

  if reason0 /= Ok
  then do
    hist <- liftBaseWith (const (getChanContents hchan))
    return (History hist, reason0)
  else do
    (reason, _) <- foldM (go hchan) (reason0, env0) suffixes
    hist <- liftBaseWith (const (getChanContents hchan))
    return (History hist, reason)
  where
    go hchan (_, env) (Pair cmds1 cmds2) = do
      ((reason1, (env1, _)), (reason2, (env2, _))) <- concurrently

        -- XXX: Post-conditions not checked, so we can pass in initModel here...
        -- It would be better if we made executeCommands take a Maybe model
        -- instead of the boolean...

        (runStateT (executeCommands sm hchan (Pid 1) False cmds1) (env, initModel))
        (runStateT (executeCommands sm hchan (Pid 2) False cmds2) (env, initModel))
      return ( reason1 `combineReason` reason2
             , env1 <> env2
             )
      where
        combineReason :: Reason -> Reason -> Reason
        combineReason Ok r2 = r2
        combineReason r1 _  = r1

------------------------------------------------------------------------

-- | Try to linearise a history of a parallel program execution using a
--   sequential model. See the *Linearizability: a correctness condition for
--   concurrent objects* paper linked to from the README for more info.
linearise :: forall model submodel cmd m resp.
             (Show (cmd Concrete), Show (resp Concrete))
          => AdvancedStateMachine model submodel cmd m resp
          -> History cmd resp -> Logic
linearise StateMachine { transition,  postcondition, initModel } = go . unHistory
  where
    go :: [(Pid, HistoryEvent cmd resp)] -> Logic
    go [] = Top
    go es = exists (interleavings es) (step initModel)

    step :: model Concrete -> Tree (Operation cmd resp) -> Logic
    step _model (Node (Crash _cmd _err _pid) _roses) =
      error "Not implemented yet, see issue #162 for more details."
    step model (Node (Operation cmd resp _) roses)   =
      postcondition model cmd resp .&&
        exists' roses (step (transition model cmd resp))

exists' :: Show a => [a] -> (a -> Logic) -> Logic
exists' [] _ = Top
exists' xs p = exists xs p

------------------------------------------------------------------------

-- | Takes the output of parallel program runs and pretty prints a
--   counterexample if any of the runs fail.
prettyParallelCommands :: (MonadIO m, Rank2.Foldable cmd)
                       => (Show (cmd Concrete), Show (resp Concrete))
                       => ParallelCommands cmd
                       -> [(History cmd resp, Logic)] -- ^ Output of 'runParallelCommands'.
                       -> PropertyM m ()
prettyParallelCommands cmds =
  mapM_ (\(hist, l) -> printCounterexample hist (logic l) `whenFailM` property (boolean l))
    where
      printCounterexample hist  (VFalse ce) = do
        print (toBoxDrawings cmds hist)
        putStrLn ""
        print (simplify ce)
        putStrLn ""
      printCounterexample _hist _
        = error "prettyParallelCommands: impossible, because `boolean l` was False."

      simplify :: Counterexample -> Counterexample
      simplify (ExistsC _ [Fst ce])       = ce
      simplify (ExistsC x (Fst ce : ces)) = ce `EitherC` simplify (ExistsC x ces)
      simplify (ExistsC _ (Snd ce : _))   = simplify ce
      simplify _                          = error "simplify: impossible,\
                                                  \ because of the structure of linearise."

-- | Draw an ASCII diagram of the history of a parallel program. Useful for
--   seeing how a race condition might have occured.
toBoxDrawings :: forall cmd resp. Rank2.Foldable cmd
              => (Show (cmd Concrete), Show (resp Concrete))
              => ParallelCommands cmd -> History cmd resp -> Doc
toBoxDrawings (ParallelCommands prefix suffixes) = toBoxDrawings'' allVars
  where
    allVars = getAllUsedVars prefix `S.union`
                foldMap (foldMap getAllUsedVars) suffixes

    toBoxDrawings'' :: Set Var -> History cmd resp -> Doc
    toBoxDrawings'' knownVars (History h) = exec evT (fmap (out . snd) <$> Fork l p r)
      where
        (p, h') = partition (\e -> fst e == Pid 0) h
        (l, r)  = partition (\e -> fst e == Pid 1) h'

        out :: HistoryEvent cmd resp -> String
        out (Invocation cmd vars)
          | vars `S.isSubsetOf` knownVars = show (S.toList vars) ++ " ← " ++ show cmd
          | otherwise                     = show cmd
        out (Response resp) = show resp
        out (Exception err) = err

        toEventType :: History' cmd resp -> [(EventType, Pid)]
        toEventType = map go
          where
            go e = case e of
              (pid, Invocation _ _) -> (Open,  pid)
              (pid, Response   _)   -> (Close, pid)
              (pid, Exception  _)   -> (Close, pid)

        evT :: [(EventType, Pid)]
        evT = toEventType (filter (\e -> fst e `Prelude.elem` map Pid [1, 2]) h)

getAllUsedVars :: Rank2.Foldable cmd => Commands cmd -> Set Var
getAllUsedVars = foldMap (\(Command cmd _) -> getUsedVars cmd) . unCommands
