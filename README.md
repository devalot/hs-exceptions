# Stupid



Haskell is great about forcing programmers to deal with problems at
compile time. That said, it's still possible to write code which may not
work at runtime. Especially with *partial* functions.

The function below will throw an exception at runtime if it's given an
empty list. This is because `head` is a partial function and only works
with non-empty lists.



~~~~ {.haskell include="src/head.hs" token="stupid"}
stupid :: [Int] -> Int
stupid xs = head xs + 1
~~~~

# Better



A better approach is to avoid the use of `head` and pattern match the
list directly. The function below is *total* since it can handle lists
of any length (including infinite lists).

Of course, if the list or its head is bottom (⊥) then this function will
throw an exception when the patterns are evaluated. We'll talk about
bottom in a bit.



~~~~ {.haskell include="src/head.hs" token="better"}
better :: [Int] -> Maybe Int
better []    = Nothing
better (x:_) = Just (x + 1)
~~~~

# Reusing Existing Functions



This is the version I like most because it reuses existing functions
that are well tested.

The `listToMaybe` function comes with the Haskell Platform. It takes a
list and returns its head in a `Just`. If the list is empty it returns
`Nothing`. Alternatively you can use the `headMay` function from the
[Safe](http://hackage.haskell.org/package/safe) package.



~~~~ {.haskell include="src/head.hs" token="reuse"}
reuse :: [Int] -> Maybe Int
reuse = fmap (+1) . listToMaybe
~~~~

# Providing Error Messages



Another popular type when dealing with failure is `Either` which allows
you to return a value with an error. It's common to include an error
message using the `Left` constructor.

Beyond `Maybe` and `Either` it's also common to define your own type
that indicates success or failure. We won't discuss this further.



~~~~ {.haskell include="src/head.hs" token="either"}
withError :: [Int] -> Either String Int
withError []    = Left "this is awkward"
withError (x:_) = Right (x + 1)
~~~~

# Maybe and Either

`Maybe` and `Either` are also monads!





# Maybe and IO





~~~~ {.haskell include="src/maybe.hs" token="size"}
size :: FilePath -> IO (Maybe Integer)
size f = do
  exist <- fileExist f
  
  if exist
    then Just <$> fileSize f
    else return Nothing
~~~~

# Maybe and IO





~~~~ {.haskell include="src/maybe.hs" token="add"}
add :: FilePath -> FilePath -> IO (Maybe Integer)
add f1 f2 = do
  s1 <- size f1
  s2 <- size f2

  if isNothing s1 || isNothing s2
    then return Nothing
    else return ((+) <$> s1 <*> s2)
~~~~

# MaybeT





~~~~ {.haskell include="src/maybe.hs" token="sizeT"}
sizeT :: FilePath -> MaybeT IO Integer
sizeT f = do
  exist <- lift (fileExist f)
           
  if exist
    then lift (fileSize f)
    else mzero
~~~~

# Maybe T





~~~~ {.haskell include="src/maybe.hs" token="addT"}
addT :: FilePath -> FilePath -> IO (Maybe Integer)
addT f1 f2 = runMaybeT $ do
  s1 <- sizeT f1
  s2 <- sizeT f2
  return (s1 + s2)
~~~~

# Either and IO





~~~~ {.haskell include="src/either.hs" token="size"}
size :: FilePath -> IO (Either String Integer)
size f = do
  exist <- fileExist f

  if exist
    then Right <$> fileSize f
    else return . Left $ "no such file: " ++ f
~~~~

# Either and IO





~~~~ {.haskell include="src/either.hs" token="add"}
add :: FilePath -> FilePath -> IO (Either String Integer)
add f1 f2 = do
  s1 <- size f1

  case s1 of
    Left _  -> return s1
    Right x -> do
      s2 <- size f2
      case s2 of
        Left _  -> return s2
        Right y -> return . Right $ x + y
~~~~

# ErrorT





~~~~ {.haskell include="src/either.hs" token="sizeT"}
sizeT :: FilePath -> ErrorT String IO Integer
sizeT f = do
  exist <- lift $ fileExist f

  if exist
    then lift $ fileSize f
    else fail $ "no such file: " ++ f
~~~~

# ErrorT





~~~~ {.haskell include="src/either.hs" token="addT"}
addT :: FilePath -> FilePath -> IO (Either String Integer)
addT f1 f2 = runErrorT $ do
  s1 <- sizeT f1
  s2 <- sizeT f2
  return (s1 + s2)
~~~~

# Hidden/Internal ErrorT





~~~~ {.haskell include="src/either.hs" token="addT'"}
addT' :: FilePath -> FilePath -> IO (Either String Integer)
addT' f1 f2 = runErrorT $ do
  s1 <- ErrorT $ size f1
  s2 <- ErrorT $ size f2
  return (s1 + s2)
~~~~

# Type Inhabitants



The `Bool` type is a very simple type that doesn't use any type
variables and only has 2 data constructors. This means that there can
only be 2 unique values for this type. Or does it?



~~~~ {.haskell include="src/bottom.hs" token="bool"}
data Bool = False | True
~~~~

# Bottom (⊥)



All types in Haskell support a value called bottom. This means that the
`Bool` type actually has 3 possible values. Exceptions and
non-termination are examples of bottom values.

The list below illustrates that bottom values aren't a problem until
they're evaluated.



~~~~ {.haskell include="src/bottom.hs" token="list"}
bools :: [Bool]
bools = [False, True, undefined]
~~~~

# Creating ⊥



Haskell includes 2 functions for creating bottom values: `undefined` and
`error`. In GHC `undefined` is implemented using `error` and `error`
throws an exception.

You can create a bottom value directly by writing a non-terminating
function.



~~~~ {.haskell}
-- Raise exceptions in GHC:
undefined :: a
error :: String -> a

-- Non-termination:
badBoy :: a
badBoy = badBoy
~~~~

# Catching Exceptions (Inline)



Catching exceptions is straight forward as long as you remember 2
things:

1.  You can only catch exceptions in the `IO` monad, and

2.  You need to ensure that values are evaluated because they might
    contain unevaluated exceptions.

In the example below you'll notice the use of the "`$!`" operator. This
forces evaluation to WHNF so exceptions don't sneak out of the `catch`
function as unevaluated thunks.



~~~~ {.haskell include="src/catch-throw.hs" token="inline"}
inline :: Int -> IO Int
inline x =
  catch (return $! naughtyFunction x)
        (\(_ex :: StupidException) -> return 0)
~~~~



The second argument to `catch` is a function to handle a caught
exception. GHC uses the type of the function to determine if it can
handle the caught exception. If GHC can't infer the type of the function
you'll need to add a type annotation like in the example below. This
requires the `ScopedTypeVariables` extension.

If you want to handle more than one exception type you'll need to use
something like the `catches` function. To catch all possible exceptions
you can catch the `SomeException` type since it's at the top of the
exception type hierarchy. This isn't generally wise and instead you
should use something like the `bracket` or `finally` functions.



# Catching Exceptions (w/ a Helper)



Below is another example of catching exceptions. This time a helper
function with an explicit type signature is used to handle the
exception. This allows us to avoid inline type annotations and the
`ScopedTypeVariables` extension.



~~~~ {.haskell include="src/catch-throw.hs" token="helper"}
helper :: Int -> IO Int
helper x =
  catch (return $! naughtyFunction x)
        handler
  where
    handler :: StupidException -> IO Int
    handler _ = return 0
~~~~

# Throwing Exceptions



Throwing exceptions is really easy. Unlike catching exceptions you don't
need to be in the `IO` monad to throw them. If you do happen to be in
the `IO` monad you should use the `throwIO` function instead of the
"pure" `throw` function.



~~~~ {.haskell include="src/catch-throw.hs" token="throw"}
naughtyFunction :: Int -> Int
naughtyFunction x =
  if x > 0
    then x - 1
    else throw StupidException
~~~~

# Creating Custom Exceptions



Any type can be used as an exception as long as it's an instance of the
`Exception` type class. Deriving from the `Typeable` class makes
creating the `Exception` instance trivial. However, using `Typeable`
means you need to enable the `DeriveDataTypeable` GHC extension.

You can also automatically derive the `Show` instance as with most other
types, but creating one manually allows you to write a more descriptive
message for the custom exception.



~~~~ {.haskell include="src/catch-throw.hs" token="ex"}
data StupidException = StupidException
  deriving (Typeable)

instance Show StupidException where
  show StupidException =
    "StupidException: you did something stupid"

instance Exception StupidException
~~~~

# Threads and Exceptions



Concurrency greatly complicates exception handling. The GHC runtime uses
exceptions to send various signals to threads. You also need to be very
careful with unevaluated thunks exiting from a thread when it
terminates.



Additional problems created by concurrency:

-   Exceptions are used to kill threads.

-   Exceptions are asynchronous.

-   Need to mask exceptions in critical code.

-   Probably don't want unevaluated exceptions leaking out.

# There's a Package For That

Just use the [async](http://hackage.haskell.org/package/async) package.

# Turning Exceptions into Errors





~~~~ {.haskell}
try :: Exception e => IO a -> IO (Either e a)
~~~~
