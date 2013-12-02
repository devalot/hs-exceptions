% Errors and Exceptions in Haskell
% Peter Jones <br/> <pjones@devalot.com>
% December 4, 2013

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

<div class="notes">

A better approach is to avoid the use of `head` and pattern match the
list directly.  The function below is *total* since it can handle
lists of any length (including infinite lists).

Of course, if the list or its head is bottom (⊥) then this function
will throw an exception when the patterns are evaluated.  We'll talk
about bottom in a bit.

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

# Type Inhabitants

<div class="notes">

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
-- Raise exceptions in GHC:
undefined :: a
error :: String -> a

-- Non-termination:
badBoy :: a
badBoy = badBoy
~~~

# Catching Exceptions (Inline)

<div class="notes">

Catching exceptions is straight forward as long as you remember 2
things:

  1. You can only catch exceptions in the `IO` monad, and

  2. You need to ensure that values are evaluated because they might
     contain unevaluated exceptions.

In the example below you'll notice the use of the "`$!`" operator.
This forces evaluation to WHNF so exceptions don't sneak out of the
`catch` function as unevaluated thunks.

The second argument to `catch` is a function to handle a caught
exception.  GHC uses the type of the function to determine if it can
handle the caught exception.  If GHC can't infer the type of the
function you'll need to add a type annotation like in the example
below.  This requires the `ScopedTypeVariables` extension.

If you want to handle more than one exception type you'll need to use
something like the `catches` function.  To catch all possible
exceptions you can catch the `SomeException` type since it's at the
top of the exception type hierarchy.  This isn't generally wise and
instead you should use something like the `bracket` or `finally`
functions.

</div>

~~~{.haskell include="src/catch-throw.hs" token="inline"}
~~~

# Catching Exceptions (w/ a Helper)

<div class="notes">

Below is another example of catching exceptions.  This time a helper
function with an explicit type signature is used to handle the
exception.  This allows us to avoid type annotations and the
`ScopedTypeVariables` extension.

</div>

~~~{.haskell include="src/catch-throw.hs" token="helper"}
~~~

# Throwing Exceptions

<div class="notes">

Throwing exceptions is really easy.  Unlike catching exceptions you
don't need to be in the `IO` monad to throw them.  If you do happen to
be in the `IO` monad you can use the `throwIO` function instead of the
"pure" `throw` function.

</div>

~~~{.haskell include="src/catch-throw.hs" token="throw"}
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

  * Exceptions are used to kill threads.

  * Exceptions are asynchronous.

  * You need to mask exceptions in critical code.

# There's a Package For That

Just use the [async][] package.

[async]: http://hackage.haskell.org/package/async

# Turning Exceptions into Errors

<div class="notes">
</div>
