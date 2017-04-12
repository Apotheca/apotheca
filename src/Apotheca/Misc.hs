module Apotheca.Misc where

tagWith :: (a -> b) -> a -> (b, a)
tagWith f a = (f a, a)

toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing


-- Experimental

-- like ($) but applies 2 arguments
infixr 0 $+
($+) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f $+ g = curry (f . uncurry g) -- Or ((f .) . g)

-- like ($) but applies to the second argument
infixr 0 $-
($-) :: (a -> b -> c) -> b -> (a -> c)
f $- b = flip f b

-- These two feel wierd - need a .- and a .+ instead?
--  These use like `(f .$- b . g) a` instead of something like `(f .- g) a b`
--  See https://hackage.haskell.org/package/pointless-fun-1.1.0.6/docs/Data-Function-Pointless.html
-- Opposite precedence for chaining
-- infixr 8 .$+
-- (.$+) = ($+)
--
-- infixr 8 .$-
-- (.$-) = ($-)
