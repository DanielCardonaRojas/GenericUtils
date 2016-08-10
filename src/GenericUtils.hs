module GenericUtils where

import Control.Monad
import Control.Arrow
import Control.Applicative

-- | Double composition
(|>) = flip ($)
(<.) = flip (.)
(<..) = (.) . (.)
(..>) = flip (<..)

------------ Arrowised Operator ------------
bothAndMerge  f g h = f &&& g >>> arr (uncurry h)

-------------- Logic Operator --------------
-- | Higher order logic operators
-- An alternative definition based on monoid would be 
-- (&.&) = getAll (fmap All f <> fmap All g)
-- (&.&) f g =  getAll . foldMap (All .) [f,g]
(&.&) f g = bothAndMerge f g (&&)
(|.|) f g = bothAndMerge f g (||)

compareWith f x y = compare (f x) (f y)
eqBy f x y = (f x) == (f y)

------------ List operations --------------
splitEvery _ [] = []
splitEvery 0 ls = [ls]
splitEvery n list = first : (splitEvery n rest) 
    where (first,rest) = splitAt n list

-- | 'innerZip' takes a list and returns a a list of adjacent elements
-- If the input list has odd length then the last element is trimmed.
innerZip :: [a] -> [(a,a)]
innerZip = map tuplify . filter ((== 2) . length) . splitEvery 2  where tuplify [x,y] = (x,y)

------------ Conversion ------------
toMaybe p x = iff p Just (const Nothing)
toSingleton = maybe [] (:[]) 

------------ Control Combinators  ------------
-- Ex: iff ((> 3) &.& even) (+ 2) (* 3) 4
-- data Person = Person {name :: String, age :: Int} deriving (Show,Eq)
-- Ex: (age `is` even) &.& (name `is` palindrome) where palindrome x = x == reverse x
-- In contrast with \x -> if x > 3 && even x then x + 2 else x * 3
is :: (t -> a) -> (a -> Bool) -> t -> Bool
is = (.>)
iff p f g = \x -> if p x then f x else g x

----------- Function Combinators -------------
applyNTimes n f = (!! n) . iterate f

fmap2 :: (Functor f , Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

---------- Generalized Zipping --------------
zipWith' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
zipWith' = liftA2

zip' :: Applicative f => f a -> f b -> f (a,b)
zip' = zipWith' (,)

unzip' :: Functor f => f (a,b) -> (f a, f b)
unzip' = fmap fst &&& fmap snd
