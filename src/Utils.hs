module Utils (firstMaybe, following, getParameters) where

import Data.List.Split
import Data.List

firstMaybe :: [a] -> Maybe a
firstMaybe [] = Nothing
firstMaybe a = Just $ head a

following :: Eq a => a -> [a] -> [a]
following a b =
  case elemIndex a b of
   Nothing -> b
   Just index -> if result == [] then b else result
                                         where result = drop (index+1) b

getParameters :: String -> [String]
getParameters a = case firstMaybe . drop 1 $ splitOn "?" a of
                   Nothing -> []
                   Just result -> splitOn "&" result
