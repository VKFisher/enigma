module Common where

import Data.Foldable (toList)
import qualified Data.Set as Set
import Data.List (nub)

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where
    set = Set.fromList list
    
countOf :: (Foldable f, Eq a) => a -> f a -> Int
countOf value = length . filter (== value) . toList

getDuplicates :: Eq a => [a] -> [a]
getDuplicates l = nub . filter (\x -> countOf x l > 1) $ l