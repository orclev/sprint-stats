{-# LANGUAGE RankNTypes, ScopedTypeVariables, TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Util where

import Data.List

toFloat :: Real a => a -> Float
toFloat = fromRational . toRational

foldlassoc' :: forall a. Int -> a -> (Int -> a -> a -> a) -> [a] -> a
foldlassoc' n x f xs = snd $ foldl' f' x' xs'
  where
    x' :: (Int, a)
    x' = (n, x)
    xs' :: [(Int, a)]
    xs' = zip [1 + n .. length xs + n] xs
    f' :: (Int, a) -> (Int, a) -> (Int, a)
    f' (_, a) (i, k) = (undefined, f i a k)

foldl1assoc' :: forall a. (Int -> a -> a -> a) -> [a] -> a
foldl1assoc' f xs = foldlassoc' 1 (head xs) f (tail xs)

mma :: Fractional a => Int -> a -> a -> a
mma n p v = ((n' - 1) * p + v) / n'
  where
    n' = fromRational $ toRational n

calcMma :: Fractional a => [a] -> a
calcMma xs = foldl1assoc' mma xs