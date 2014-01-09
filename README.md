# Exceptions

![Exceptions](img/explosion.jpg)\
 Creative Commons image by [gynti](http://www.flickr.com/photos/gynti)



Most languages make a distinction between values that represent failure
(errors) and the mechanism to abort computations and unwind the stack
(exceptions.) Haskell is unique in that the type system makes it safe
and easy to build failure into types instead of lumping everything into
something like `NULL` or `-1`.

It also stands out by supporting exceptions through a library of
functions and types instead of directly in the syntax of the language.
The fact that there's no dedicated keywords for exceptions might seem
weird until you discover how flexible and expressive Haskell is.

This presentation aims to show how closely related errors and exceptions
are, and how to keep them separate.



# Haskell Exceptions

-   No dedicated syntax

-   Very limited in Haskell 2010

-   Expanded by GHC

# Type Inhabitants



In order to understand how exceptions work we first need to talk about
type inhabitants and bottom.

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
-- Raises exceptions in GHC:
undefined :: a
error :: String -> a

-- Non-termination:
badBoy :: a
badBoy = badBoy
~~~~

# Catching Exceptions (Inline)



Catching exceptions is straight forward as long as you remember that you
can only catch exceptions in the `IO` monad.



~~~~ {.haskell include="src/catch-throw.hs" token="inline"}
inline :: Int -> IO Int
inline x =
  catch (shortFuse x)
        (\(_ex :: StupidException) -> return 0)
~~~~



The second argument to `catch` is a function to handle a caught
exception. GHC uses the type of the function to determine if it can
handle the caught exception. If GHC can't infer the type of the function
you'll need to add a type annotation like in the example above. This
requires the `ScopedTypeVariables` extension.

If you want to handle more than one exception type you'll need to use
something like the `catches` function. To catch all possible exceptions
you can catch the `SomeException` type since it's at the top of the
exception type hierarchy. This isn't generally wise and instead you
should use something like the `bracket` or `finally` functions.

One interesting thing to note is that GHC differs from Haskell 2010 with
regards to `catch`. Haskell 2010 states that `catch` should catch all
exceptions regardless of their type. Probably because those exceptions
would all be `IOError`s.



# Catching Exceptions (w/ a Helper)



Below is another example of catching exceptions. This time a helper
function with an explicit type signature is used to handle the
exception. This allows us to avoid inline type annotations and the
`ScopedTypeVariables` extension.



~~~~ {.haskell include="src/catch-throw.hs" token="helper"}
helper :: Int -> IO Int
helper x =
  catch (shortFuse x)
        handler
  where
    handler :: StupidException -> IO Int
    handler _ = return 0
~~~~

# Throwing Exceptions



Throwing exceptions is really easy, although you must be in the `IO`
monad to do so. Haskell 2010 provides a set of functions for creating
and raising exceptions.



*Haskell 2010:*

~~~~ {.haskell}
-- Create an exception.
userError :: String -> IOError

-- Raise an exception.
ioError :: IOError -> IO a

-- fail from the IO Monad is both.
fail = ioError . userError :: String -> IO a
~~~~

# Throwing Exceptions



GHC adds on to Haskell 2010 with functions like `throwIO` and `throw`.
The `throw` function allows you to raise an exception in pure code and
is considered to be a misfeature.



*GHC:*

~~~~ {.haskell include="src/catch-throw.hs" token="throwIO"}
shortFuse :: Int -> IO Int
shortFuse x =
  if x > 0
    then return (x - 1)
    else throwIO StupidException
~~~~

# Throwing from Pure Code



As mentioned above, GHC adds a `throw` function that allows you to raise
an exception from pure code. Unfortunately this makes it very difficult
to catch.



~~~~ {.haskell include="src/catch-throw.hs" token="throw"}
naughtyFunction :: Int -> Int
naughtyFunction x =
  if x > 0
    then x - 1
    else throw StupidException
~~~~

# Catching Exceptions From `throw`



You need to ensure that values are evaluated because they might contain
unevaluated exceptions.

In the example below you'll notice the use of the "`$!`" operator. This
forces evaluation to WHNF so exceptions don't sneak out of the `catch`
function as unevaluated thunks.



~~~~ {.haskell include="src/catch-throw.hs" token="forced"}
forced :: Int -> IO Int
forced x =
  catch (return $! naughtyFunction x)
        (\(_ex :: StupidException) -> return 0)
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

-   Exceptions are used to kill threads

-   Exceptions are asynchronous

-   Need to mask exceptions in critical code

-   Probably don't want unevaluated exceptions leaking out

# There's a Package For That

Just use the [async](http://hackage.haskell.org/package/async) package.

# Errors (Instead of Exceptions)

-   Explicit

-   Checked by the compiler

-   Way better than `NULL` or `-1`

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

Prefer errors to exceptions.



A better approach is to avoid the use of `head` and pattern match the
list directly. The function below is *total* since it can handle lists
of any length (including infinite lists).

Of course, if the list or its head is bottom (⊥) then this function will
throw an exception when the patterns are evaluated.



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



If you have several functions that return one of these types you can use
`do` notation to sequence them and abort the entire block on the first
failure. This allows you to write short code that implicitly checks the
return value of every function.

Things tend to get a bit messy when you mix monads though...



# Maybe and IO



The code below demonstrates mixing two monads, `IO` and `Maybe`. Clearly
we want to be able to perform I/O but we also want to use the `Maybe`
type to signal when a file doesn't exist. This isn't too complicated,
but what happens when we want to use the power of the `Maybe` monad to
short circuit a computation when we encounter a `Nothing`?



~~~~ {.haskell include="src/maybe.hs" token="size"}
size :: FilePath -> IO (Maybe Integer)
size f = do
  exist <- fileExist f
  
  if exist
    then Just <$> fileSize f
    else return Nothing
~~~~

# Maybe and IO



Because `IO` is the outer monad and we can't do without it, we sort of
lose the superpowers of the `Maybe` monad.



~~~~ {.haskell include="src/maybe.hs" token="add"}
add :: FilePath -> FilePath -> IO (Maybe Integer)
add f1 f2 = do
  s1 <- size f1
  case s1 of
    Nothing -> return Nothing
    Just x  -> size f2 >>= \s2 ->
      case s2 of
        Nothing -> return Nothing
        Just y  -> return . Just $ x + y
~~~~

# MaybeT



Using the `MaybeT` monad transformer we can make `IO` the inner monad
and restore the `Maybe` goodness. We don't really see the benefit in the
`sizeT` function but note that its complexity remains about the same.



~~~~ {.haskell include="src/maybe.hs" token="sizeT"}
sizeT :: FilePath -> MaybeT IO Integer
sizeT f = do
  exist <- lift (fileExist f)
           
  if exist
    then lift (fileSize f)
    else mzero
~~~~

# MaybeT



The real payoff comes in the `addT` function. Compare with the `add`
function above.



~~~~ {.haskell include="src/maybe.hs" token="addT"}
addT :: FilePath -> FilePath -> IO (Maybe Integer)
addT f1 f2 = runMaybeT $ do
  s1 <- sizeT f1
  s2 <- sizeT f2
  return (s1 + s2)
~~~~

# Either and IO



This version using `Either` is nearly identical to the `Maybe` version
above. The only difference is that we can now report the name of the
file which doesn't exist.



~~~~ {.haskell include="src/either.hs" token="size"}
size :: FilePath -> IO (Either String Integer)
size f = do
  exist <- fileExist f

  if exist
    then Right <$> fileSize f
    else return . Left $ "no such file: " ++ f
~~~~

# Either and IO



To truly abort the `add` function when one of the files doesn't exist
we'd need to replicate the nested `case` code from the `Maybe` example.
Here I'm cheating and using `Either`'s applicative instance. However,
this doesn't short circuit the second file test if the first fails.



~~~~ {.haskell include="src/either.hs" token="add"}
add :: FilePath -> FilePath -> IO (Either String Integer)
add f1 f2 = do
  s1 <- size f1
  s2 <- size f2
  return ((+) <$> s1 <*> s2)
~~~~

# ErrorT



The `ErrorT` monad transformer is to `Either` what `MaybeT` is to
`Maybe`. Again, changing `size` to work with a transformer isn't that
big of a deal.



~~~~ {.haskell include="src/either.hs" token="sizeT"}
sizeT :: FilePath -> ErrorT String IO Integer
sizeT f = do
  exist <- lift $ fileExist f

  if exist
    then lift $ fileSize f
    else fail $ "no such file: " ++ f
~~~~

# ErrorT



But it makes a big difference in the `addT` function.



~~~~ {.haskell include="src/either.hs" token="addT"}
addT :: FilePath -> FilePath -> IO (Either String Integer)
addT f1 f2 = runErrorT $ do
  s1 <- sizeT f1
  s2 <- sizeT f2
  return (s1 + s2)
~~~~

# Hidden/Internal ErrorT



The really interesting thing is that we didn't actually have to change
`size` at all. We could have retained the non-transformer version and
used the `ErrorT` constructor to lift the `size` function into the
transformer. The `MaybeT` constructor can be used in a similar way.



~~~~ {.haskell include="src/either.hs" token="addT'"}
addT' :: FilePath -> FilePath -> IO (Either String Integer)
addT' f1 f2 = runErrorT $ do
  s1 <- ErrorT $ size f1
  s2 <- ErrorT $ size f2
  return (s1 + s2)
~~~~

# Turning Exceptions into Errors



The `try` function allows us to turn exceptions into errors in the form
of `IO` and `Either`, or as you now know, `ErrorT`.

It's not hard to see how flexible exception handling in Haskell is, in
no small part due to it not being part of the syntax. Non-strict
evaluation is the other major ingredient.



~~~~ {.haskell}
try :: Exception e => IO a -> IO (Either e a)

-- Which is equivalent to:
try :: Exception e => IO a -> ErrorT e IO a
~~~~

# Final Thought

-   **Prefer Errors to Exceptions!**

-   **Don't Write/Use Partial Functions!**


