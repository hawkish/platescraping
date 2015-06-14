{-# LANGUAGE FlexibleContexts #-}
module Utils (following, getParameterAt, getElementAt, extractFst, extractSnd, extractTrd) where

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
getParameterAt' a n = getElementAt list n
                      where list = splitOn "&" a
                         
getElementAt :: [a] -> Int -> Maybe a
getElementAt a n = if n > length a - 1
                      then Nothing
                      else Just $ a !! n

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
                                
myParser :: Parsec.Parsec String () String
myParser = do
  letters <- Parsec.many1 Parsec.letter
  return $ letters

extractFst :: (a, b, c) -> a
extractFst (a,_,_) = a

extractSnd :: (a, b, c) -> b
extractSnd (_,b,_) = b

extractTrd :: (a, b, c) -> c
extractTrd (_,_,c) = c


