module Test.Language.LCX.Lex
( test
) where

import Data.List (fromFoldable)
import Data.Maybe (Maybe(Just, Nothing))
import Language.LCX.Lex
import Prelude
import Test.Assert (assert)

just :: String -> Array Lexeme -> Boolean
just s ls = lex s == Just (fromFoldable ls)

test = do
  assert $ just "" []

  assert $ just "let x: bool = true;"
                [Let, Identifier "x", Colon, Identifier "bool", Equals, True, Semicolon]
