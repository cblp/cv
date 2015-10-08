{-# LANGUAGE TypeOperators #-}

module Data.Tuple.X where

type a :- b = (a, b)

(-:) :: a -> b -> (a, b)
(-:) = (,)
