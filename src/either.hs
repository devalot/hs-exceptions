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
import Control.Monad.Error
import System.Environment
import System.IO
import System.Posix.Files (fileExist)

--------------------------------------------------------------------------------
fileSize :: FilePath -> IO Integer
fileSize f = withFile f ReadMode hFileSize

--------------------------------------------------------------------------------
-- {BEGIN: size}
size :: FilePath -> IO (Either String Integer)
size f = do
  exist <- fileExist f

  if exist
    then Right <$> fileSize f
    else return . Left $ "no such file: " ++ f
-- {END}

--------------------------------------------------------------------------------
-- {BEGIN: add}
add :: FilePath -> FilePath -> IO (Either String Integer)
add f1 f2 = do
  s1 <- size f1
  s2 <- size f2
  return ((+) <$> s1 <*> s2)
-- {END}

--------------------------------------------------------------------------------
-- {BEGIN: sizeT}
sizeT :: FilePath -> ErrorT String IO Integer
sizeT f = do
  exist <- lift $ fileExist f

  if exist
    then lift $ fileSize f
    else fail $ "no such file: " ++ f
-- {END}

--------------------------------------------------------------------------------
-- {BEGIN: addT}
addT :: FilePath -> FilePath -> IO (Either String Integer)
addT f1 f2 = runErrorT $ do
  s1 <- sizeT f1
  s2 <- sizeT f2
  return (s1 + s2)
-- {END}

--------------------------------------------------------------------------------
-- {BEGIN: addT'}
addT' :: FilePath -> FilePath -> IO (Either String Integer)
addT' f1 f2 = runErrorT $ do
  s1 <- ErrorT $ size f1
  s2 <- ErrorT $ size f2
  return (s1 + s2)
-- {END}

--------------------------------------------------------------------------------
main :: IO ()
main = do
  args  <- getArgs

  case args of
    (x:y:_) -> do print =<< add   x y
                  print =<< addT  x y
                  print =<< addT' x y
    _       -> putStrLn "read the source code dork"
