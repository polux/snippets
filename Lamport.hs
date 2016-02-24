-- Copyright 2016 Google Inc. All Rights Reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE TemplateHaskell, GADTs, KindSignatures, TypeSynonymInstances, FlexibleInstances #-}

import Data.List
import Control.Monad
import Control.Monad.Operational
import Lens.Micro
import Lens.Micro.TH

{- Actors Library -}

type ProcessId = Int

type Message m = (ProcessId, m)

data ActorInstruction s m :: * -> * where
  GetState :: ActorInstruction s m s
  SetState :: s -> ActorInstruction s m ()
  SendMessage :: ProcessId -> m -> ActorInstruction s m ()
  WaitForMessage :: ActorInstruction s m (Message m)

type ActorProgram s m a = Program (ActorInstruction s m) a

getState = singleton GetState
setState s = singleton (SetState s)
sendMessage pid m = singleton (SendMessage pid m)
waitForMessage = singleton WaitForMessage

type Queue m = [Message m]

data Actor s m =
  Actor {
    _actorProgram :: ProgramView (ActorInstruction s m) (),
    _actorState   :: s,
    _actorQueue   :: Queue m
  }

makeLenses ''Actor

stepActor :: Actor s m -> (Actor s m, Maybe (Message m))
stepActor (Actor program state queue) = stepActor' program
  where stepActor' (GetState :>>= is) = (Actor (view (is state)) state queue, Nothing)
        stepActor' (SetState x :>>= is) = (Actor (view (is ())) x queue, Nothing)
        stepActor' (SendMessage pid m :>>= is) = (Actor (view (is ())) state queue, Just (pid, m))
        stepActor' (WaitForMessage :>>= is) | (m:ms) <- queue = (Actor (view (is m)) state ms, Nothing)
        stepActor' _ = (Actor program state queue, Nothing)

step :: ProcessId -> [Actor s m] -> [Actor s m]
step pid actors | Just (qid, m) <- message = actors' & ix qid . actorQueue %~ (++[(pid, m)])
                | otherwise = actors'
  where (actor', message) = stepActor (actors !! pid)
        actors' = actors & ix pid .~ actor'

run :: [ProcessId] -> [Actor s m] -> [[Actor s m]]
run pids actors = scanl (flip step) actors pids

simpleRun :: [ProcessId] -> [(ActorProgram s m (), s)] -> [[Actor s m]]
simpleRun pids pairs = run pids [Actor (view p) s [] | (p, s) <- pairs]

{- Pretty-Printing -}

instance (Show s, Show m) => Show (ActorInstruction s m a) where
  show GetState = "_ <- getState"
  show (SetState x) = "setState " ++ show x
  show (SendMessage pid m) = "sendMessage " ++ show pid ++ " " ++ show m
  show WaitForMessage = "_ <- waitForMessage"

instance (Show s, Show m, Show a) => Show (ProgramView (ActorInstruction s m) a) where
  show (Return x) = "return " ++ show x
  show (i :>>= is) = show i ++ "; ..."

pretty :: (Show s, Show m) => [ProcessId] -> [[Actor s m]] -> String
pretty pids (t:ts) = unlines (prettyActors t : zipWith prettyStep (pids) ts)
  where prettyStep pid actors = "==========\nadvance " ++ show pid ++ "\n==========\n\n" ++ prettyActors actors
        prettyActors actors = intercalate "\n" (zipWith prettyActor [0..] actors)
        prettyActor pid (Actor program state queue) = unlines [
           "pid " ++ show pid ++ ":",
           "  program: " ++ show program,
           "  state: " ++ show state,
           "  queue: " ++ show queue]

{- Example -}

-- The program of a simple actor with a state of type Int that sends and
-- receives messages of type String. Calling getState, setState, sendMessage or
-- waitForMessage potentially yields control back to the scheduler.
program :: ProcessId -> ActorProgram Int String ()
program otherPid = forever $ do
  counter <- getState
  sendMessage otherPid ("ping " ++ show counter)
  (_, _) <- waitForMessage
  setState (counter + 1)

-- Two actors : (program 1) with initial state 0, and (program 0) with initial 0.
actors = [(program 1, 0), (program 0, 0)]

-- The sequence in which we want to execute the actors, in that case:
-- actor 0, then actor 1, then actor 0, then actor 1, ...
executionSequence = 0:1:executionSequence

-- The execution trace: every actor's state at each step of the execution
trace = simpleRun executionSequence actors

-- Pretty-print the execution trace
main = putStrLn (pretty executionSequence trace)

