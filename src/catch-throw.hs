{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

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
import Data.Typeable
import System.Environment

--------------------------------------------------------------------------------
-- {BEGIN: ex}
data StupidException = StupidException
  deriving (Typeable)

instance Show StupidException where
  show StupidException =
    "StupidException: you did something stupid"

instance Exception StupidException
-- {END}

--------------------------------------------------------------------------------
-- {BEGIN: throw}
naughtyFunction :: Int -> Int
naughtyFunction x =
  if x > 0
    then x - 1
    else throw StupidException
-- {END}

--------------------------------------------------------------------------------
-- {BEGIN: inline}
inline :: Int -> IO Int
inline x =
  catch (return $! naughtyFunction x)
        (\(_ex :: StupidException) -> return 0)
-- {END}

--------------------------------------------------------------------------------
-- {BEGIN: helper}
helper :: Int -> IO Int
helper x =
  catch (return $! naughtyFunction x)
        handler
  where
    handler :: StupidException -> IO Int
    handler _ = return 0
-- {END}

--------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- fmap read <$> getArgs

  print =<< case args of
    []    -> inline 0
    (0:_) -> helper 0
    (1:_) -> helper 1
    (x:_) -> inline x
