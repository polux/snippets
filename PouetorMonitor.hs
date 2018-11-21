#!/usr/bin/env stack
{-
  stack
  --resolver lts-12.19
  --install-ghc
  runghc
  --package base
  --package bytestring
  --package irc-client
  --package irc-colors
  --package text
  --package transformers
  --package lens
-}

-- Copyright 2018 Google Inc. All Rights Reserved.
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

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Network.IRC.Client
import System.Environment (getArgs)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.IntMap.Strict as M
import Control.Concurrent.STM

newtype Histogram = Histogram { histogram :: M.IntMap Int }
  deriving (Show)

updateHistogram :: Histogram -> Int -> Histogram
updateHistogram (Histogram h) n = Histogram (M.insertWith (+) n 1 h)

display :: Histogram -> T.Text
display (Histogram h) = T.pack $ show $ M.toAscList h

len :: T.Text -> Int
len txt = T.length (T.filter (=='U') txt)

showStats = EventHandler (matchType _Privmsg) $ \src (target, Right msg) ->
  when (msg == "!histo") $ do
    histo <- getIRCState >>= snapshot userState
    replyTo src (display histo)

readPouet chan = EventHandler (matchType _Privmsg) $ \src (target, Right msg) ->
  case src of
    (Channel c sender) | c == chan && sender == "pouetor" -> do
      tvar <- get userState <$> getIRCState
      liftIO . atomically $ do
        h <- readTVar tvar
        writeTVar tvar (updateHistogram h (len msg))
    _ -> return ()

run :: B.ByteString -> Int -> T.Text -> T.Text -> IO ()
run host port nick channel =
  runClient conn cfg (Histogram M.empty)
 where
  conn = plainConnection host port
  cfg = defaultInstanceConfig nick
          & handlers %~ ([readPouet channel, showStats]++)
          & channels .~ [channel]

main = do
  [host, port, nick, channel] <- getArgs
  run (B.pack host) (read port) (T.pack nick) (T.pack channel)
