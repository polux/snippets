#!/usr/bin/env stack
{-
   stack 
   --resolver lts-7.17
   --install-ghc
   runghc
   --package base
   --package colorful-monoids
   --package megaparsec
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

module Main where

import           Control.Monad
import           Data.Functor
import           Data.List (intersperse)
import           Data.Monoid
import qualified Data.Monoid.Colorful as C
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String

-- DATATYPES

data Sea = Sea [Chunk]
  deriving Show

data Chunk = TomChunk Tom | WaterChunk String
  deriving Show

data Term = Appl String [Term] | Var String
  deriving Show

data Tom = Match Term [Rule]
         | BackQuote Term
  deriving Show

data Rule = Rule Term Sea
  deriving Show

data Border = OBrace | OMatch | OBackQuote | EndOfScope
  deriving Show

-- PRETTY-PRINTING

joinWith sep values = mconcat (intersperse sep values)

class Pretty a where
  pretty :: a -> C.Colored String

instance Pretty Sea where
  pretty (Sea chunks) = mconcat (map pretty chunks)

instance Pretty Chunk where
  pretty (TomChunk tom) = C.Fg C.Red (pretty tom)
  pretty (WaterChunk water) = C.Fg C.Blue (C.Value water)


instance Pretty Term where
  pretty (Var x) = C.Value x
  pretty (Appl f args) = C.Value f <> "(" <> joinWith ", " (map pretty args) <> ")"

instance Pretty Tom where
  pretty (Match subject rules) =
    "%match("
    <> pretty subject
    <> ") { "
    <> joinWith " " (map pretty rules)
    <> " }"
  pretty (BackQuote term) = "`" <> pretty term

instance Pretty Rule where
  pretty (Rule lhs rhs) = pretty lhs <> " -> {" <> C.Fg C.Blue (pretty rhs) <> "}"

-- LEXER

spaceConsumer =
  L.space
    (spaceChar $> ())
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")
lexeme  = L.lexeme spaceConsumer
symbol = L.symbol spaceConsumer

kMatch = symbol "%match"
kArrow = symbol "->"
kLBrace = symbol "{"
kRBrace = symbol "}"
kLParen = symbol "("
kRParen = symbol ")"
kComma = symbol ","
kBackQuote = symbol "`"
kIdentifer = lexeme (liftM2 (:) letterChar (many alphaNumChar))

-- PARSER

braces = between kLBrace kRBrace
parens = between kLParen kRParen

border :: Parser Border
border = choice
  [ kLBrace $> OBrace
  , kMatch $> OMatch
  , kBackQuote $> OBackQuote
  , kRBrace $> EndOfScope
  , eof $> EndOfScope
  ]

anyCharUntil :: Parser a -> Parser (String, a)
anyCharUntil p = end <|> cons
  where end = do b <- lookAhead p
                 return ("", b)
        cons = do c <- anyChar
                  (str, b) <- anyCharUntil p
                  return (c:str, b)

sea :: Parser Sea
sea = liftM Sea chunks

chunks :: Parser [Chunk]
chunks = do
  (water, token) <- anyCharUntil border
  liftM (WaterChunk water :) $
    case token of
      OBrace -> do
        body <- braces chunks
        tail <- chunks
        return (WaterChunk "{" : body ++ WaterChunk "}" : tail)
      OMatch -> do
        body <- match
        tail <- chunks
        return (TomChunk body : tail)
      OBackQuote -> do
        body <- backQuote
        tail <- chunks
        return (TomChunk body : tail)
      EndOfScope -> do
        return []

match :: Parser Tom
match = do
  _ <- kMatch
  subject <- parens term
  rules <- braces (many rule)
  return (Match subject rules)

rule :: Parser Rule
rule = do
  lhs <- term
  _ <- kArrow
  rhs <- braces sea
  return (Rule lhs rhs)

backQuote :: Parser Tom
backQuote = do
  _ <- kBackQuote
  res <- term
  return (BackQuote res)

term :: Parser Term
term = do
  id <- kIdentifer
  (appl id) <|> (return (Var id))

   where appl fun = do
           args <- parens (term `sepBy` kComma)
           return (Appl fun args)

-- MAIN

program :: String
program = "\
\  public class Test {                            \
\    public static bool lessThan(Nat n, Nat m) {  \
\      %match(n) {                                \
\        Z()  -> { return true; }                 \
\        S(p) -> {                                \
\          %match(m) {                            \
\            Z() -> { return false; }             \
\            S(q) -> { return `lessThan(p, q); }  \
\          }                                      \
\        }                                        \
\      }                                          \
\    }                                            \
\  }                                              "

main = do
  terminal <- C.getTerm
  case parse (space *> sea <* eof) "program.tom" program of
    Left error -> putStrLn (parseErrorPretty error)
    Right result -> do
      C.printColoredS terminal (pretty result)
      putStrLn ""
