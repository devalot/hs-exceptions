% Exceptions and Errors in Haskell
% Peter Jones <br/> <pjones@devalot.com>
% January 8, 2014

# Exceptions

![Exceptions](img/explosion.jpg)\

Creative Commons image by [gynti](http://www.flickr.com/photos/gynti)

<div class="notes">

Most languages make a distinction between values that represent
failure (errors) and the mechanism to abort computations and unwind
the stack (exceptions.)  Haskell is unique in that the type system
makes it safe and easy to build failure into types instead of lumping
everything into something like `NULL` or `-1`.

It also stands out by supporting exceptions through a library of
functions and types instead of directly in the syntax of the language.
The fact that there's no dedicated keywords for exceptions might seem
weird until you discover how flexible and expressive Haskell is.

This presentation aims to show how closely related errors and
exceptions are, and how to keep them separate.

</div>

# Haskell Exceptions

  * No dedicated syntax

  * Very limited in Haskell 2010

  * Expanded by GHC

# Type Inhabitants

<div class="notes">

In order to understand how exceptions work we first need to talk about
type inhabitants and bottom.

The `Bool` type is a very simple type that doesn't use any type
variables and only has 2 data constructors.  This means that there can
only be 2 unique values for this type.  Or does it?

</div>

~~~{.haskell include="src/bottom.hs" token="bool"}
~~~

# Bottom (⊥)

<div class="notes">

All types in Haskell support a value called bottom.  This means that
the `Bool` type actually has 3 possible values.  Exceptions and
non-termination are examples of bottom values.

The list below illustrates that bottom values aren't a problem until
they're evaluated.

</div>

~~~{.haskell include="src/bottom.hs" token="list"}
~~~

# Creating ⊥

<div class="notes">

Haskell includes 2 functions for creating bottom values: `undefined`
and `error`.  In GHC `undefined` is implemented using `error` and
`error` throws an exception.

You can create a bottom value directly by writing a non-terminating
function.

</div>

~~~{.haskell}
-- Raises exceptions in GHC:
undefined :: a
error :: String -> a

-- Non-termination:
badBoy :: a
badBoy = badBoy
~~~

# Catching Exceptions (Inline)

<div class="notes">

Catching exceptions is straight forward as long as you remember that
you can only catch exceptions in the `IO` monad.

</div>

~~~{.haskell include="src/catch-throw.hs" token="inline"}
~~~

<div class="notes">

The second argument to `catch` is a function to handle a caught
exception.  GHC uses the type of the function to determine if it can
handle the caught exception.  If GHC can't infer the type of the
function you'll need to add a type annotation like in the example
above.  This requires the `ScopedTypeVariables` extension.

If you want to handle more than one exception type you'll need to use
something like the `catches` function.  To catch all possible
exceptions you can catch the `SomeException` type since it's at the
top of the exception type hierarchy.  This isn't generally wise and
instead you should use something like the `bracket` or `finally`
functions.

One interesting thing to note is that GHC differs from Haskell 2010
with regards to `catch`.  Haskell 2010 states that `catch` should
catch all exceptions regardless of their type.  Probably because those
exceptions would all be `IOError`s.


</div>

# Catching Exceptions (w/ a Helper)

<div class="notes">

Below is another example of catching exceptions.  This time a helper
function with an explicit type signature is used to handle the
exception.  This allows us to avoid inline type annotations and the
`ScopedTypeVariables` extension.

</div>

~~~{.haskell include="src/catch-throw.hs" token="helper"}
~~~

# Throwing Exceptions

<div class="notes">

Throwing exceptions is really easy, although you must be in the `IO`
monad to do so.  Haskell 2010 provides a set of functions for creating
and raising exceptions.

</div>

*Haskell 2010:*

~~~{.haskell}
-- Create an exception.
userError :: String -> IOError

-- Raise an exception.
ioError :: IOError -> IO a

-- fail from the IO Monad is both.
fail = ioError . userError :: String -> IO a
~~~

# Throwing Exceptions

<div class="notes">

GHC adds on to Haskell 2010 with functions like `throwIO` and `throw`.
The `throw` function allows you to raise an exception in pure code and
is considered to be a misfeature.

</div>

*GHC:*

~~~{.haskell include="src/catch-throw.hs" token="throwIO"}
~~~

# Throwing from Pure Code

<div class="notes">

As mentioned above, GHC adds a `throw` function that allows you to
raise an exception from pure code.  Unfortunately this makes it very
difficult to catch.

</div>

~~~{.haskell include="src/catch-throw.hs" token="throw"}
~~~

# Catching Exceptions From `throw`

<div class="notes">

You need to ensure that values are evaluated because they might
contain unevaluated exceptions.

In the example below you'll notice the use of the "`$!`" operator.
This forces evaluation to WHNF so exceptions don't sneak out of the
`catch` function as unevaluated thunks.

</div>

~~~{.haskell include="src/catch-throw.hs" token="forced"}
~~~

# Creating Custom Exceptions

<div class="notes">

Any type can be used as an exception as long as it's an instance of
the `Exception` type class.  Deriving from the `Typeable` class makes
creating the `Exception` instance trivial.  However, using `Typeable`
means you need to enable the `DeriveDataTypeable` GHC extension.

You can also automatically derive the `Show` instance as with most
other types, but creating one manually allows you to write a more
descriptive message for the custom exception.

</div>

~~~{.haskell include="src/catch-throw.hs" token="ex"}
~~~

# Threads and Exceptions

<div class="notes">

Concurrency greatly complicates exception handling.  The GHC runtime
uses exceptions to send various signals to threads.  You also need to
be very careful with unevaluated thunks exiting from a thread when it
terminates.

</div>

Additional problems created by concurrency:

  * Exceptions are used to kill threads

  * Exceptions are asynchronous

  * Need to mask exceptions in critical code

  * Probably don't want unevaluated exceptions leaking out

# There's a Package For That

Just use the [async][] package.

[async]: http://hackage.haskell.org/package/async

# Errors (Instead of Exceptions)

  * Explicit

  * Checked by the compiler

  * Way better than `NULL` or `-1`

# Stupid

<div class="notes">

Haskell is great about forcing programmers to deal with problems at
compile time.  That said, it's still possible to write code which may
not work at runtime.  Especially with *partial* functions.

The function below will throw an exception at runtime if it's given an
empty list.  This is because `head` is a partial function and only
works with non-empty lists.

</div>

~~~{.haskell include="src/head.hs" token="stupid"}
~~~

# Better

Prefer errors to exceptions.

<div class="notes">

A better approach is to avoid the use of `head` and pattern match the
list directly.  The function below is *total* since it can handle
lists of any length (including infinite lists).

Of course, if the list or its head is bottom (⊥) then this function
will throw an exception when the patterns are evaluated.

</div>

~~~{.haskell include="src/head.hs" token="better"}
~~~

# Reusing Existing Functions

<div class="notes">

This is the version I like most because it reuses existing functions
that are well tested.

The `listToMaybe` function comes with the Haskell Platform.  It takes
a list and returns its head in a `Just`.  If the list is empty it
returns `Nothing`.  Alternatively you can use the `headMay` function
from the [Safe][safe] package.

</div>

~~~{.haskell include="src/head.hs" token="reuse"}
~~~

[safe]: http://hackage.haskell.org/package/safe

# Providing Error Messages

<div class="notes">

Another popular type when dealing with failure is `Either` which
allows you to return a value with an error.  It's common to include an
error message using the `Left` constructor.

Beyond `Maybe` and `Either` it's also common to define your own type
that indicates success or failure.  We won't discuss this further.

</div>

~~~{.haskell include="src/head.hs" token="either"}
~~~

# Maybe and Either

`Maybe` and `Either` are also monads!

<div class="notes">

If you have several functions that return one of these types you can
use `do` notation to sequence them and abort the entire block on the
first failure.  This allows you to write short code that implicitly
checks the return value of every function.

Things tend to get a bit messing when you mix monads though...

</div>

# Maybe and IO

<div class="notes">

The code below demonstrates mixing two monads, `IO` and `Maybe`.
Clearly we want to be able to perform I/O but we also want to use the
`Maybe` type to signal when a file doesn't exist.  This isn't too
complicated, but what happens when we want to use the power of the
`Maybe` monad to short circuit a computation when we encounter a
`Nothing`?

</div>

~~~{.haskell include="src/maybe.hs" token="size"}
~~~

# Maybe and IO

<div class="notes">

Because `IO` is the outer monad and we can't do without it, we sort of
loose the superpowers of the `Maybe` monad.

</div>

~~~{.haskell include="src/maybe.hs" token="add"}
~~~

# MaybeT

<div class="notes">

Using the `MaybeT` monad transformer we can make `IO` the inner monad
and restore the `Maybe` goodness.  We don't really see the benefit in
the `sizeT` function but note that its complexity remains about the
same.

</div>

~~~{.haskell include="src/maybe.hs" token="sizeT"}
~~~

# Maybe T

<div class="notes">

The real payoff comes in the `addT` function.  Compare with the `add`
function above.

</div>

~~~{.haskell include="src/maybe.hs" token="addT"}
~~~

# Either and IO

<div class="notes">

This version using `Either` is nearly identical to the `Maybe` version
above.  The only difference is that we can now report the name of the
file which doesn't exist.

</div>

~~~{.haskell include="src/either.hs" token="size"}
~~~


# Either and IO

<div class="notes">

To truly abort the `add` function when one of the files doesn't exist
we'd need to replicate the nested `case` code from the `Maybe`
example.  Here I'm cheating and using `Either`'s applicative instance.
However, this doesn't short circuit the second file test if the first
fails.

</div>

~~~{.haskell include="src/either.hs" token="add"}
~~~

# ErrorT

<div class="notes">

The `ErrorT` monad transformer is to `Either` what `MaybeT` is to
`Maybe`.  Again, changing `size` to work with a transformer isn't that
big of a deal.

</div>

~~~{.haskell include="src/either.hs" token="sizeT"}
~~~

# ErrorT

<div class="notes">

But it makes a big difference in the `addT` function.

</div>

~~~{.haskell include="src/either.hs" token="addT"}
~~~

# Hidden/Internal ErrorT

<div class="notes">

The really interesting thing is that we didn't actually have to change
`size` at all.  We could have retained the non-transformer version and
used the `ErrorT` constructor to lift the `size` function into the
transformer.  The `MaybeT` constructor can be used in a similar way.

</div>

~~~{.haskell include="src/either.hs" token="addT'"}
~~~

# Turning Exceptions into Errors

<div class="notes">

The `try` function allows us to turn exceptions into errors in the
form of `IO` and `Either`, or as you now know, `ErrorT`.

It's not hard to see how flexible exception handling in Haskell is, in
no small part due to it not being part of the syntax.  Non-strict
evaluation is the other major ingredient.

</div>

~~~{.haskell}
try :: Exception e => IO a -> IO (Either e a)

-- Which is equivalent to:
try :: Exception e => IO a -> ErrorT e IO a
~~~

# Final Thought

**Prefer Errors to Exceptions!**
