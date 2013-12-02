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
</div>

~~~{.haskell include="src/bottom.hs" token="bool"}
~~~

# Bottom (⊥)

<div class="notes">

Illustrates that bottom values aren't a problem until they're
evaluated.

</div>

~~~{.haskell include="src/bottom.hs" token="list"}
~~~

# Creating ⊥

<div class="notes">
</div>

~~~{.haskell}
undefined :: a
error :: String -> a

badBoy :: a
badBoy = badBoy
~~~

# Catching Exceptions

<div class="notes">
</div>

# Throwing Exceptions

<div class="notes">
</div>

# Threads and Exceptions

<div class="notes">
</div>

Exceptions are used to:

  * Kill threads

  * Foo Bar

# Exception Evaluation

<div class="notes">
</div>

# There's a Package For That

<div class="notes">
</div>

[async]: http://hackage.haskell.org/package/async
