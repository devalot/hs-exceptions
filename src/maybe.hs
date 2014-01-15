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
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import System.Environment
import System.IO
import System.Posix.Files (fileExist)

--------------------------------------------------------------------------------
fileSize :: FilePath -> IO Integer
fileSize f = withFile f ReadMode hFileSize

--------------------------------------------------------------------------------
-- {BEGIN: size}
size :: FilePath -> IO (Maybe Integer)
size f = do
  exist <- fileExist f

  if exist
    then Just <$> fileSize f
    else return Nothing
-- {END}

--------------------------------------------------------------------------------
-- {BEGIN: add}
add :: FilePath -> FilePath -> IO (Maybe Integer)
add f1 f2 = do
  s1 <- size f1
  case s1 of
    Nothing -> return Nothing
    Just x  -> size f2 >>= \s2 ->
      case s2 of
        Nothing -> return Nothing
        Just y  -> return . Just $ x + y
-- {END}

--------------------------------------------------------------------------------
-- {BEGIN: sizeT}
sizeT :: FilePath -> MaybeT IO Integer
sizeT f = do
  exist <- lift (fileExist f)

  if exist
    then lift (fileSize f)
    else mzero
-- {END}

--------------------------------------------------------------------------------
-- {BEGIN: addT}
addT :: FilePath -> FilePath -> IO (Maybe Integer)
addT f1 f2 = runMaybeT $ do
  s1 <- sizeT f1
  s2 <- sizeT f2
  return (s1 + s2)
-- {END}

--------------------------------------------------------------------------------
sizeT' :: (MonadIO m) => FilePath -> MaybeT m Integer
sizeT' f = do
  guard =<< liftIO (fileExist f)
  liftIO (fileSize f)

--------------------------------------------------------------------------------
addT' :: FilePath -> FilePath -> IO (Maybe Integer)
addT' f1 f2 = runMaybeT $ do
  s1 <- sizeT' f1
  s2 <- sizeT' f2
  return (s1 + s2)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  args  <- getArgs

  case args of
    (x:y:_) -> do print =<< add   x y
                  print =<< addT  x y
                  print =<< addT' x y
    _       -> putStrLn "read the source code dork"
