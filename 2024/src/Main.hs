{-# LANGUAGE TupleSections #-}
module Main where

import Data.List
import System.IO
import Control.Monad
import Data.Bifunctor (bimap)
import qualified Data.HashMap.Strict as HM


main :: IO ()
main = do
  withFile "src/test.txt" ReadMode $ \h -> do 
    lineNums <- lines <$> hGetContents h
    let [xs, ys] = transpose $ map words lineNums
    let (m1, m2) = join bimap (sort . map read) (xs, ys)
    let hashy = HM.fromListWith (+) $ map ((,1)) m2
    print $ distance m1 m2
    print $ similarity hashy m1

distance :: [Integer] -> [Integer] -> Integer
distance xs ys = sum . map (\(a,b) -> abs (a-b)) $ zip xs ys

similarity :: HM.HashMap Integer Integer -> [Integer] -> Integer
similarity hashy = sum . map (\x -> x * (HM.findWithDefault 0 x hashy)) 

