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
import Control.Applicative
import Control.Exception
import Data.Maybe
import System.Environment

--------------------------------------------------------------------------------
-- {BEGIN: stupid}
stupid :: [Int] -> Int
stupid xs = head xs + 1
-- {END}

--------------------------------------------------------------------------------
-- {BEGIN: better}
better :: [Int] -> Maybe Int
better []    = Nothing
better (x:_) = Just (x + 1)
-- {END}

--------------------------------------------------------------------------------
-- {BEGIN: reuse}
reuse :: [Int] -> Maybe Int
reuse = fmap (+1) . listToMaybe
-- {END}

--------------------------------------------------------------------------------
-- {BEGIN: either}
withError :: [Int] -> Either String Int
withError []    = Left "this is awkward"
withError (x:_) = Right (x + 1)
-- {END}

--------------------------------------------------------------------------------
handler :: SomeException -> IO ()
handler _ = putStrLn "ERROR"

--------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- fmap read <$> getArgs
  catch (print $ stupid args) handler
  print $ better args
  print $ reuse args
  print $ withError args
