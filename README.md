# Stupid

<div class="notes">

Haskell is great about forcing programmers to deal with problems at
compile time. That said, it's still possible to write code which may not
work at runtime. Especially with *partial* functions.

</div>

~~~~ {.haskell include="src/head.hs" token="stupid"}
stupid :: [Int] -> Int
stupid xs = head xs + 1
~~~~

# Better

<div class="notes">
</div>

~~~~ {.haskell include="src/head.hs" token="better"}
better :: [Int] -> Maybe Int
better []    = Nothing
better (x:_) = Just (x + 1)
~~~~

# Reusing Existing Functions

<div class="notes">
</div>

~~~~ {.haskell include="src/head.hs" token="reuse"}
reuse :: [Int] -> Maybe Int
reuse = fmap (+1) . listToMaybe
~~~~
