#!/usr/bin/env stack
{-
  stack
  --resolver lts-7.16
  --install-ghc
  runghc
  --package base
  --package bytestring
  --package irc-client
  --package irc-colors
  --package normaldistribution
  --package random
  --package text
  --package transformers
  --
  -hide-all-packages
-}

-- Copyright 2017 Google Inc. All Rights Reserved.
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
import Control.Monad.Trans.Class (lift)
import Data.Monoid ((<>))
import Data.Random.Normal (normalIO')
import Data.Text.IRC.Color (rainbow)
import Network.IRC.Client
import System.Environment (getArgs)
import System.Random (randomRIO)

run :: B.ByteString -> Int -> T.Text -> T.Text -> IO ()
run host port nick channel = do
  conn <- connect host port 1
  let cfg = defaultIRCConf nick
  let cfg' = cfg {
    _eventHandlers = khanHandler : _eventHandlers cfg,
    _channels = [channel]
  }
  start conn cfg'

khanHandler = EventHandler
  { _description = "Says Kha...an when invoked with !khan"
  , _matchType = EPrivmsg
  , _eventFunc = sayKhan
  }

sayKhan :: UnicodeEvent -> IRC ()
sayKhan e@(Event _ _ (Privmsg _ (Right msg))) | "!khan" `T.isInfixOf` msg = do
  n <- lift (ceiling <$> normalIO' (5 :: Double, 2))
  reply e (rainbow ("Kh" <> T.replicate n "a" <> "n!"))
sayKhan _ = return ()

main = do
  [host, port, nick, channel] <- getArgs
  run (B.pack host) (read port) (T.pack nick) (T.pack channel)
