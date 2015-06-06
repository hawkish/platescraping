{-# LANGUAGE FlexibleContexts #-}
module Utils (following, getParameterAt) where

import Data.List.Split
import Control.Exception
import Data.List
import Data.Maybe
import qualified Text.Parsec as Parsec

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

import Control.Applicative
-- alias Parsec.parse for more concise usage.
parse rule text = Parsec.parse rule "(source)" text

h = "/tinglysning/forespoerg/bilbogen/bilbogen.xhtml;TDK_JSESSIONID=E4gvjvzPV_6nECcCdeE8POCn_QtrIGoS0LU6---cO4zQBWaWw0bX!-478567563!1440987210?_afPfm=-11ykkgvvlm&some=234"


following :: Eq a => a -> [a] -> [a]
following a b =
  case elemIndex a b of
   Nothing -> b
   Just index -> if result == [] then b else result
                                         where result = drop (index+1) b

getParametersAsString :: String -> Maybe String
getParametersAsString a = listToMaybe . drop 1 $ splitOn "?" a

getParameterAt' :: String -> Int -> Maybe String
getParameterAt' a n = if n > limit
                     then Nothing
                     else Just $ list !! n
                          where list = splitOn "&" a
                                limit = length list - 1

-- Avoiding case expression ladder with Monad. 
getParameterAt :: String -> Int -> Maybe String
getParameterAt a n = do
  a1 <- getParametersAsString a
  a2 <- getParameterAt' a1 n
  return a2

getCookie :: String -> Maybe String
getCookie a = do
  a1 <- listToMaybe . drop 1 $ splitOn ";" a
  a2 <- listToMaybe $ splitOn "?" a1
  a3 <- listToMaybe . drop 1 $ splitOn "=" a2
  return a3
                                
{--
getParametersAsString2 :: Maybe String -> Maybe String
getParametersAsString2 a =
  case a of
       Nothing -> Nothing
       Just a -> firstMaybe . drop 1 $ splitOn "?" a

getFirstParameter'2 :: Maybe String -> Maybe String
getFirstParameter'2 a =
  case a of
   Nothing -> Nothing
   Just a -> firstMaybe $ splitOn "&" a

-- Avoiding case expression ladder with Functor. 
getFirstParameter2 :: Maybe String -> Maybe String
--getFirstParameter2 = fmap getFirstParameter'2 getParametersAsString2  
getFirstParameter2 = getFirstParameter'2 <$> getParametersAsString2  
--}

myParser :: Parsec.Parsec String () String
myParser = do
  letters <- Parsec.many1 Parsec.letter
  return $ letters





