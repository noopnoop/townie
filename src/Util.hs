-- Util.hs
-- Various helpful functions
module Util where
import           Data.List                      ( nub )
import           GHC.Exts                       ( sortWith )

mte :: e -> Maybe a -> Either e a
mte e Nothing  = Left e
mte _ (Just x) = Right x

count :: Eq a => a -> [a] -> Int
count _ []      = 0
count x (h : t) = if x /= h then count x t else 1 + count x t

tally :: Eq a => [a] -> [(a, Int)]
tally xs = reverse $ sortWith snd $ nub $ map (\x -> (x, count x xs)) xs
