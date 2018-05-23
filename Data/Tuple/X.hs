{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Data.Tuple.X where

type (:-) = (,)

pattern (:-) :: a -> b -> (a, b)
pattern a :- b = (a, b)

(-:) :: a -> b -> (a, b)
(-:) = (,)
