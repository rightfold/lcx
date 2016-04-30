module Language.LCX.Lex
( Lexeme(..)
, lex
) where

import Data.Generic (class Generic, gEq)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String as String
import Data.List ((:), List)
import Data.List as List
import Prelude

data Lexeme
  = Identifier String

  | Let
  | True

  | Colon
  | Equals
  | Semicolon

derive instance genericLexeme :: Generic Lexeme
instance eqLexeme :: Eq Lexeme where eq = gEq

lex :: String -> Maybe (List Lexeme)
lex = map List.reverse <<< go List.Nil

go :: List Lexeme -> String -> Maybe (List Lexeme)
go ls st = case String.uncons st of
  Nothing -> Just ls

  Just {head: c,   tail: cs} | isSpace c -> go ls cs

  Just {head: c,   tail: cs} | isIdentifierHead c ->
    let name = String.fromChar c <> (String.takeWhile isIdentifierTail cs)
        lexeme = case name of
                   "let"   -> Let
                   "true"  -> True
                   other   -> Identifier other
     in go (lexeme : ls) (String.dropWhile isIdentifierTail cs)

  Just {head: ':', tail: cs} -> go (Colon : ls) cs
  Just {head: '=', tail: cs} -> go (Equals : ls) cs
  Just {head: ';', tail: cs} -> go (Semicolon : ls) cs

  Just _ -> Nothing

isSpace :: Char -> Boolean
isSpace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

isIdentifierHead :: Char -> Boolean
isIdentifierHead c
  | c >= 'a' && c <= 'z' = true
  | c >= 'A' && c <= 'Z' = true
  | c == '_'             = true
  | otherwise            = false

isIdentifierTail :: Char -> Boolean
isIdentifierTail c
  | isIdentifierHead c   = true
  | c >= '0' && c <= '9' = true
  | otherwise            = false
