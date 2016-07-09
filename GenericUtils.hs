import Control.Monad
import Control.Arrow

-- | Double composition
(|>) = flip ($)
(<.) = flip (.)
(<..) = (.) . (.)
(..>) = flip (<..)

------------ Arrowised Operator ------------
bothAndMerge  f g h = f &&& g >>> arr (uncurry h)

-------------- Logic Operator --------------
-- | Higher order logic operators
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
iff p f g = \x -> if p x then f x else g x

