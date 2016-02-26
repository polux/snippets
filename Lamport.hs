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
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Operational
import Control.Monad.Loops
import Lens.Micro
import Lens.Micro.TH
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as Q

{- Actors Library -}

type ProcessId = Int

type Message m = (ProcessId, m)

data ActorInstruction s m :: * -> * where
  GetState :: ActorInstruction s m s
  SetState :: s -> ActorInstruction s m ()
  SendMessage :: ProcessId -> m -> ActorInstruction s m ()
  WaitForMessage :: ActorInstruction s m (Message m)

type ActorProgram s m a = Program (ActorInstruction s m) a

type Queues m = M.Map ProcessId [m]

data Actor s m =
  Actor {
    _actorProgram :: ActorProgram s m (),
    _actorState   :: s,
    _actorQueues  :: Queues m
  }

makeLenses ''Actor

enqueue :: ProcessId -> m -> Queues m -> Queues m
enqueue pid m = M.insertWith (flip (++)) pid [m]

dequeue :: ProcessId -> Queues m -> Maybe (m, Queues m)
dequeue pid queues
  | Just (m:ms) <- M.lookup pid queues = Just (m, update ms)
  | otherwise = Nothing
  where update [] = M.delete pid queues
        update ms = M.insert pid ms queues

stepActor :: ProcessId -> Actor s m -> (Actor s m, Maybe (Message m))
stepActor sid (Actor program state queues) = stepActor' (view program)
  where stepActor' (GetState :>>= is) = (Actor (is state) state queues, Nothing)
        stepActor' (SetState x :>>= is) = (Actor (is ()) x queues, Nothing)
        stepActor' (SendMessage pid m :>>= is) = (Actor (is ()) state queues, Just (pid, m))
        stepActor' (WaitForMessage :>>= is)
            | Just (m, queues') <- dequeue sid queues = (Actor (is (sid, m)) state queues', Nothing)
        stepActor' _ = (Actor program state queues, Nothing)

step :: (ProcessId, ProcessId) -> [Actor s m] -> [Actor s m]
step (pid, sid) actors | Just (rid, m) <- message = actors' & ix rid . actorQueues %~ enqueue pid m
                       | otherwise = actors'
  where (actor', message) = stepActor sid (actors !! pid)
        actors' = actors & ix pid .~ actor'

run :: [(ProcessId, ProcessId)] -> [Actor s m] -> [[Actor s m]]
run pids actors = scanl (flip step) actors pids

simpleRun :: [(ProcessId, ProcessId)] -> [(ActorProgram s m (), s)] -> [[Actor s m]]
simpleRun pids pairs = run pids [Actor p s M.empty | (p, s) <- pairs]

{- Helper Functions for writing ActorPrograms -}

getState = singleton GetState
setState s = singleton (SetState s)
sendMessage pid m = singleton (SendMessage pid m)
waitForMessage = singleton WaitForMessage

modify f = do { s <- getState; setState (f s) }
get l = do { s <- getState; return (s ^. l) }

infix 4 .=, %=, +=, ++=

l .= x = modify (l .~ x)
l %= f = modify (l %~ f)
l += x = l %= (+x)
l ++= x = l %= (++x)

{- Pretty-Printing -}

instance (Show s, Show m) => Show (ActorInstruction s m a) where
  show GetState = "_ <- getState"
  show (SetState x) = "setState " ++ show x
  show (SendMessage pid m) = "sendMessage " ++ show pid ++ " " ++ show m
  show WaitForMessage = "_ <- waitForMessage"

instance (Show s, Show m, Show a) => Show (ProgramView (ActorInstruction s m) a) where
  show (Return x) = "return " ++ show x
  show (i :>>= is) = show i ++ "; ..."

pretty :: (Show s, Show m) => [(ProcessId, ProcessId)] -> [[Actor s m]] -> String
pretty pids (t:ts) = unlines (prettyActors t : zipWith prettyStep (pids) ts)
  where prettyStep pid actors = banner ("advance " ++ show pid) ++ "\n\n" ++ prettyActors actors
        banner x = "=============\n" ++ x ++ "\n============="
        prettyActors actors = unlines (zipWith prettyActor [0..] actors)
        prettyActor pid (Actor program state queues) =
          unlines [ "pid " ++ show pid ++ ":"
                  , "  program: " ++ show (view program)
                  , "  state: " ++ show state
                  , "  queues: " ++ prettyQueues (M.toList queues) ]
        prettyQueues queues = "{" ++ intercalate ", " (map prettyQueue queues) ++ "}"
        prettyQueue (pid, queue) = show pid ++ ": [" ++ intercalate ", " (map show queue) ++ "]"

{- Example -}

data Msg = RequestResource | ReleaseResource | Ack
  deriving Show

data State = State {
    _clock :: Int,
    _lastSeenTimestamps :: M.Map ProcessId Int,
    _requestQueue :: [(Int, ProcessId)] -- always sorted
  }
  deriving Show

makeLenses ''State

addToRequestQueue ts pid =
  requestQueue %= \queue -> sort ((ts,pid):queue)

rmFromRequestQueue pid =
  requestQueue %= filter (\(_, qid) -> qid /= pid)

sendMessage' pid m = do
  clock += 1
  c <- get clock
  sendMessage pid (c, m)

waitForMessage' = do
  clock += 1
  (ts, m) <- waitForMessage
  c <- get clock
  when (ts > c) (clock .= ts)
  return (ts, m)

ownsTheResource myPid rq lastSeen =
  not (null rq) &&
  snd (head rq) == myPid &&
  all (> fst (head rq)) (M.elems lastSeen)

iOwnTheResource myPid =
  ownsTheResource myPid <$> get requestQueue <*> get lastSeenTimestamps

program :: ProcessId -> [ProcessId] -> ActorProgram State (Int, Msg) ()
program myPid otherPids = forever $ do
  iOwn <- iOwnTheResource myPid
  if iOwn
    then do
      rmFromRequestQueue myPid
      forM_ otherPids $ \pid -> do
        sendMessage' pid ReleaseResource
    else do
      forM_ otherPids $ \pid -> do
        sendMessage' pid RequestResource
      ts <- get clock
      addToRequestQueue ts myPid
      whileM_ (not <$> iOwnTheResource myPid) $ do
        (pid, (ots, msg)) <- waitForMessage'
        lastSeenTimestamps %= M.insert pid ots
        case msg of
          RequestResource -> do
            addToRequestQueue ots pid
            sendMessage' pid Ack
          ReleaseResource -> do
            rmFromRequestQueue pid
          _ -> return ()

-- Two actors : (program 1) with initial state (State 0 0), and (program 0)
-- with initial state (State 0 0).
actors = [
  (program 0 [1,2], State 0 (M.fromList [(1,-1), (2,-1)]) [(-1,0)]),
  (program 1 [0,2], State 0 (M.fromList [(0,-1), (2,-1)]) [(-1,0)]),
  (program 2 [0,1], State 0 (M.fromList [(0,-1), (1,-1)]) [(-1,0)])]


-- We define an alias for lists of pairs of pids that represent execution
-- sequences. (The first element of a pair is the id of the process to advance,
-- the second element is the process it should receive a message from if such a
-- message is waiting and the next execution step is to wait for a messa ge.) It
-- allows us to define custom instances for Arbitrary and Show.
newtype ExecutionSequence = ExecutionSequence [(ProcessId, ProcessId)]

instance Q.Arbitrary ExecutionSequence where
  arbitrary = ExecutionSequence <$> Q.listOf ((,) <$> Q.elements [0, 1, 2] <*> Q.elements [0, 1, 2])
  shrink (ExecutionSequence pids) = ExecutionSequence <$> Q.shrink pids

instance Show ExecutionSequence where
  show (ExecutionSequence ns) = unlines [show ns, pretty ns (simpleRun ns actors)]


property1 (ExecutionSequence pids) = all property' (simpleRun pids actors)
  where property' actors = length (filter id (zipWith owns [0..] actors)) <= 1
        owns pid actor = ownsTheResource pid (actor ^. actorState.requestQueue) (actor ^. actorState.lastSeenTimestamps)

{-
allpids = 0:1:2:allpids
executionSequence = concatMap (\pid -> [(pid, 0), (pid, 1), (pid, 2)]) allpids
trace = simpleRun (take 200 executionSequence) actors

main = putStrLn (pretty executionSequence trace)
-}

main = do
  Q.quickCheckWith (Q.stdArgs { Q.maxSuccess = 10000, Q.maxSize = 10000 }) property1
