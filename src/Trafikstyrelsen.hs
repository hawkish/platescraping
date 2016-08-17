{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Trafikstyrelsen (getSurveyorRapports) where

import Network.HTTP.Client
import Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Utils (getOpenTags, dequote, getTagTexts)
import Text.HTML.TagSoup (fromAttrib, Tag)
import SurveyorRapportType (initSurveyorRapport, SurveyorRapport)

getSurveyorRapports :: T.Text -> IO [SurveyorRapport]
getSurveyorRapports a = do
  a1 <- getVINHTML a
    -- Getting link(s) to the surveyor rapport(s).
  if isResult a1
    then do
    let a2 = parseSurveyorLinks a1
    -- Using mapM :: (a -> mb) -> [a] -> m[b]
    -- In this case: (a -> IO(b)) -> [a] -> IO[b]
    -- No need to return because result is already in IO.
    -- This return an empty [] if no SurveyorRapport is found.
    mapM getSurveyorRapport a2
    else error "Ingen søgeresultat fra Trafikstyrelsen.dk."

getSurveyorRapport :: T.Text -> IO SurveyorRapport
getSurveyorRapport a = do
  a1 <- getSurveyorHTML a
  return $ initSurveyorRapport a1

isResult :: T.Text -> Bool
isResult = null . filter (T.isInfixOf "Ingen resultater") . getTagTexts

parseSurveyorLinks :: T.Text -> [T.Text]
parseSurveyorLinks = filterLink . splitAtQuote . filterLocationHref . getOnClick . getOpenTags

filterLink :: [T.Text] -> [T.Text]
filterLink = filter (T.isInfixOf "/Sider") 

splitAtQuote :: [T.Text] -> [T.Text]
splitAtQuote = concat . map (T.split (== '\"'))

filterLocationHref :: [T.Text] -> [T.Text]
filterLocationHref = filter (T.isInfixOf "location.href") 

getOnClick :: [Tag T.Text] -> [T.Text]
getOnClick = dequote . map (fromAttrib "onclick")

getVINHTML :: T.Text -> IO T.Text
getVINHTML a = do
  manager <- liftIO $ newManager defaultManagerSettings
  let baseUrl = T.pack "http://selvbetjening.trafikstyrelsen.dk/Sider/resultater.aspx?Reg="
  let url = baseUrl `T.append` a
  html <- doGetRequest manager url :: IO T.Text
  return html

getSurveyorHTML :: T.Text -> IO T.Text
getSurveyorHTML a = do
  manager <- liftIO $ newManager defaultManagerSettings
  let baseUrl = T.pack "http://selvbetjening.trafikstyrelsen.dk"
  let url = baseUrl `T.append` a
  html <- doGetRequest manager url :: IO T.Text
  return html

doGetRequest :: Manager -> T.Text -> IO T.Text
doGetRequest manager url = do
  initReq <- liftIO $ parseUrl $ T.unpack url
  let req = initReq {
        method = "GET"
        }
  resp <- httpLbs req manager
  return (TL.toStrict $ TLE.decodeUtf8 $ responseBody resp)
