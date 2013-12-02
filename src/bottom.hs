{-

This file is part of the Haskell presentation hs-exceptions. It is
subject to the license terms in the LICENSE file found in the
top-level directory of this distribution and at
https://github.com/devalot/hs-exceptions/new/master?filename=LICENSE. No
part of this presentation, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import Prelude hiding (Bool(..))

--------------------------------------------------------------------------------
-- {BEGIN: bool}
data Bool = False | True
-- {END}
          deriving (Show)

--------------------------------------------------------------------------------
-- {BEGIN: list}
bools :: [Bool]
bools = [False, True, undefined]
-- {END}

--------------------------------------------------------------------------------
main :: IO ()
main = print (head bools)
