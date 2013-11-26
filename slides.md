% Errors and Exceptions in Haskell
% Peter Jones
% December 4, 2013

# Stupid

<div class="notes">

Haskell is great about forcing programmers to deal with problems at
compile time.  That said, it's still possible to write code which may
not work at runtime.  Especially with *partial* functions.

</div>

~~~{.haskell include="src/head.hs" token="stupid"}
~~~

# Better

<div class="notes">
</div>

~~~{.haskell include="src/head.hs" token="better"}
~~~

# Reusing Existing Functions

<div class="notes">
</div>

~~~{.haskell include="src/head.hs" token="reuse"}
~~~
