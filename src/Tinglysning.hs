{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tinglysning (getLandRegister) where

import Text.HTML.TagSoup (parseTags, (~==), fromAttrib, isTagOpenName)
import Network.HTTP.Client

--Linux
import Network.HTTP.Client.OpenSSL
import qualified OpenSSL.Session as SSL

--OSX
--import Network.HTTP.Client.TLS

import Network.HTTP.Types.Header

import Data.List
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Utils (getParameterAt, getElementAt, getTagTexts)
import LandRegisterType (initLandRegister, LandRegister)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe


-- Using the MaybeT monad transformer to handle IO(Maybe a)
-- https://hackage.haskell.org/package/transformers-0.5.1.0/docs/Control-Monad-Trans-Maybe.html
getLandRegister :: T.Text -> IO (Maybe LandRegister)
--getLandRegister vin = do
  --manager <- newManager tlsManagerSettings
getLandRegister vin = withOpenSSL $ do
  manager <- newManager $ opensslManagerSettings SSL.context
  putStrLn "Doing requests..."
  runMaybeT $ do
    -- Transforming Maybe to MaybeT and drawing values from IO
    -- MaybeT :: m (Maybe a) -> MaybeT m a
    (_afPfm, viewState1, cookieList1) <- MaybeT $ doFstRequest manager
    (_afPfm2, cookieList2) <- MaybeT $ doSndRequest manager _afPfm viewState1 cookieList1
    (viewState2, rangeStart, listItemValue, cookieList3) <- MaybeT $ doTrdRequest manager vin _afPfm2 viewState1 cookieList2
    (html, cookieList4) <- MaybeT $ doFrthRequest manager _afPfm2 rangeStart viewState2 listItemValue cookieList3
    -- Return to IO monad and use runMaybe to transform to maybe.
    -- return :: Monad m => a -> m a
    -- runMaybeT :: MaybeT m a -> m (Maybe a)
    return $ initLandRegister vin html


doFrthRequest :: Manager -> T.Text -> T.Text -> T.Text -> T.Text -> [Cookie] -> IO (Maybe (T.Text, [Cookie]))
doFrthRequest manager _afPfm rangeStart viewState listItemValue cookie = do
  let redirects = 1
  let url = T.pack "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogenresults.xhtml"
  let referer = url `T.append` (T.pack "?") `T.append` _afPfm
  let requestHeaders = [
          ("Host","www.tinglysning.dk"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml;application/xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8"),
          ("Referer", TE.encodeUtf8(referer)),
          ("Connection", "keep-alive")]
  let body = [
        ("content:center:bilbogenresults:bilerid:rangeStart", TE.encodeUtf8(rangeStart)),
        ("org.apache.myfaces.trinidad.faces.FORM", "j_id4"),
        ("_noJavaScript", "false"),
        ("javax.faces.ViewState", TE.encodeUtf8(viewState)),
        ("source","content:center:bilbogenresults:bilerid:0:visbildetaljer"),
        ("state", ""),
        ("value", ""),
        ("listItem", TE.encodeUtf8(listItemValue))]
  response <- doPostRequest manager url requestHeaders body _afPfm cookie redirects :: IO (T.Text, [Cookie])
  -- Just is added to make MaybeT work above. It seemed the cleanest way to proceed.
  return $ Just $ response

doTrdRequest :: Manager -> T.Text -> T.Text -> T.Text -> [Cookie] -> IO (Maybe (T.Text, T.Text, T.Text, [Cookie]))
doTrdRequest manager vin _afPfm viewState cookie = do
  let url = T.pack "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  let redirects = 1
  let requestHeaders = [
          ("Host","www.tinglysning.dk"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8"),
          ("Referer", TE.encodeUtf8(url)),
          ("Connection", "keep-alive"),
          ("Pragma", "no-cache"),
          ("Cache-Control", "no-cache")]
  let body = [
        ("soegemaade", "content:center:bilbogen:stelnrOption"),
        ("content:center:bilbogen:stelnr", TE.encodeUtf8(vin)),
        ("content:center:bilbogen:cvr", ""),
        ("content:center:bilbogen:navn", ""),
        ("content:center:bilbogen:foedselsdato", ""),
        ("bogsattest", "content:center:bilbogen:uofficiel"),
        ("org.apache.myfaces.trinidad.faces.FORM", "j_id4"),
        ("_noJavaScript", "false"),
        ("javax.faces.ViewState", TE.encodeUtf8(viewState)),
        ("source","content:center:bilbogen:j_id150")]
  response <- doPostRequest manager url requestHeaders body _afPfm cookie redirects :: IO (T.Text, [Cookie])
  return $ procTrdResponse response

isResult :: T.Text -> Bool
isResult = null . filter (T.isInfixOf "Bilen er hverken registreret i e-TL eller DMR.") . getTagTexts

procTrdResponse ::  (T.Text, [Cookie]) -> Maybe (T.Text, T.Text, T.Text, [Cookie])
procTrdResponse response = do
  let html = fst response
  let cookieList = snd response
  viewState <- filterInputViewState html
  rangeStart <- filterInputRangeStart html
  listItemValue <- getListItemValue html
  return (viewState, rangeStart, listItemValue, cookieList)

doSndRequest :: Manager -> T.Text -> T.Text -> [Cookie] -> IO (Maybe (T.Text, [Cookie]))
doSndRequest manager _afPfm viewState cookie = do
  let redirects = 1
  let url = T.pack "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  let requestHeaders = [
          ("Host","www.tinglysning.dk"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml;application/xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8"),
          ("Referer", TE.encodeUtf8(url)),
          ("Tr-XHR-Message","true"),
          ("Connection", "keep-alive"),
          ("Pragma", "no-cache"),
          ("Cache-Control", "no-cache")]
  let body = [
        ("soegemaade", "content:center:bilbogen:stelnrOption"),
        ("content:center:bilbogen:stelnr", ""),
        ("content:center:bilbogen:cvr", ""),
        ("content:center:bilbogen:navn", ""),
        ("content:center:bilbogen:foedselsdato", ""),
        ("bogsattest", "content:center:bilbogen:uofficiel"),
        ("org.apache.myfaces.trinidad.faces.FORM", "j_id4"),
        ("_noJavaScript", "false"),
        ("javax.faces.ViewState", TE.encodeUtf8(viewState)),
        ("source","content:center:bilbogen:stelnrOption"),
        ("event","autosub"),
        ("partial","true")]
  response <- doPostRequest manager url requestHeaders body _afPfm cookie redirects :: IO (T.Text, [Cookie])
  return $ procSndResponse response

procSndResponse :: (T.Text, [Cookie]) -> Maybe (T.Text, [Cookie])
procSndResponse response = do
  a1 <- filterXMLContent $ fst response
  _afPfm <- getParameterAt a1 0
  let cookieList = snd response
  return (_afPfm, cookieList)

doFstRequest :: Manager -> IO (Maybe (T.Text, T.Text, [Cookie]))
doFstRequest manager = do
  let url = T.pack "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  let requestHeaders = [
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml;application/xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Connection", "keep-alive")]
  response <- doGetRequest manager url requestHeaders :: IO (T.Text, [Cookie])
  return $ procFstResponse response

procFstResponse :: (T.Text, [Cookie]) -> Maybe (T.Text, T.Text, [Cookie])
procFstResponse response = do
  a1 <- filterForm $ fst response
  _afPfm <- getParameterAt a1 0
  let cookieList = snd response
  viewState <- filterInputViewState $ fst response
  return (_afPfm, viewState, cookieList)

filterForm :: T.Text -> Maybe T.Text
filterForm = fmap (fromAttrib "action") . listToMaybe . filter (isTagOpenName "form") . parseTags

filterInputViewState :: T.Text -> Maybe T.Text
filterInputViewState = fmap (fromAttrib "value") . listToMaybe . filter (~== ("<input name=javax.faces.ViewState" :: String)) . filter (isTagOpenName "input") . parseTags

filterXMLContent :: T.Text -> Maybe T.Text
filterXMLContent = fmap (fromAttrib "action") . listToMaybe . filter (isTagOpenName "content") . parseTags

filterAnchor :: T.Text -> Maybe T.Text
filterAnchor = fmap (fromAttrib "onclick") . listToMaybe . filter (~== ("<a name=content:center:bilbogenresults:bilerid:0:visbildetaljer" :: String)) . filter (isTagOpenName "a") . parseTags

filterInputRangeStart :: T.Text -> Maybe T.Text
filterInputRangeStart = fmap (fromAttrib "value") . listToMaybe . filter (~== ("<input name=content:center:bilbogenresults:bilerid:rangeStart" :: String)) . filter (isTagOpenName "input") . parseTags

getListItemValue :: T.Text -> Maybe T.Text
getListItemValue a = do
  a1 <- filterAnchor a
  let elem = T.splitOn "'" a1
  indexListItem <- elemIndex "listItem" elem
  let indexListItemValue = indexListItem + 2
  a2 <- getElementAt elem indexListItemValue
  return a2

doGetRequest :: Manager -> T.Text -> RequestHeaders -> IO (T.Text, [Cookie])
doGetRequest manager url requestHeadersList = do
  initReq <- liftIO $ parseUrl $ T.unpack url
  let req = initReq {
        secure = True
        , method = "GET"
        , requestHeaders = requestHeadersList
        , responseTimeout = Just 100000000
        }
  resp <- httpLbs req manager
  let cookieJar = responseCookieJar resp
  let cookieList = destroyCookieJar cookieJar
  return (TL.toStrict $ TLE.decodeUtf8 $ responseBody resp, cookieList)

doPostRequest :: Manager -> T.Text -> RequestHeaders -> [(B.ByteString, B.ByteString)] -> T.Text -> [Cookie] -> Int -> IO (T.Text, [Cookie])
doPostRequest manager baseUrl requestHeadersList body _afPfm cookie redirects = do
  let url = baseUrl `T.append` (T.pack "?") `T.append` _afPfm
  initReq <- liftIO $ parseUrl $ T.unpack url
  let req' = initReq {
        secure = True
        , method = "POST"
        , cookieJar = Just $ createCookieJar cookie
        , requestHeaders = requestHeadersList
        , redirectCount = redirects
        , responseTimeout = Just 100000000                 
        }
  let req = urlEncodedBody body $ req'
  resp <- httpLbs req manager 
  let cookieJar = responseCookieJar resp
  let cookieList = destroyCookieJar cookieJar
  return (TL.toStrict $ TLE.decodeUtf8 $ responseBody resp, cookieList)
  
