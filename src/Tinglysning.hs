{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, fromAttrib, isTagText, isTagOpenName, isTagOpen)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Control.Exception
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Utils (following, getParameterAt, extractFst, extractSnd, extractTrd)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.List
import Network
import Data.Time.Clock
import Data.Time.Calendar
import Data.Maybe
import qualified Control.Exception as E
import Network.HTTP.Types.Status (statusCode)



n = "<form id=\"j_id4\" name=\"j_id4\" style=\"margin:0px\" method=\"POST\" onkeypress=\"return _submitOnEnter(event,'j_id4');\" action=\"/tinglysning/forespoerg/bilbogen/bilbogen.xhtml;TDK_JSESSIONID=SlEuMoHouvqLGdJ_fICGH8lzaDjd6tNZ0WKFdKJ6vHtRHb7o3ZOj!1927900994!-478567563?_afPfm=-1bav6tyn4e\">\n\n&#9;<div class=\"container-logotop\">\n    <script type=\"text/javascript\" src=\"/tinglysning/js/helptext.js\"></script>\n    <script type=\"text/javascript\" src=\"/tinglysning/js/utils.js\"></script>\n"

h = "/tinglysning/forespoerg/bilbogen/bilbogen.xhtml;TDK_JSESSIONID=E4gvjvzPV_6nECcCdeE8POCn_QtrIGoS0LU6---cO4zQBWaWw0bX!-478567563!1440987210?_afPfm=-11ykkgvvlm&some=234"

url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"

a = "<input type=\"hidden\" name=\"_noJavaScript\" value=\"false\"><span id=\"tr_j_id4_Postscript\"><input type=\"hidden\" name=\"javax.faces.ViewState\" value=\"!-tcw82nrpf\"><input type=\"hidden\" name=\"source\">"

b = "<?xml version=\"1.0\" ?>\n<?Tr-XHR-Response-Type ?>\n<content action=\"/tinglysning/forespoerg/bilbogen/bilbogen.xhtml?_afPfm=-x8n0v5c5u\"> \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n<fragment>"



doFthRequest _afPfm viewState cookie = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogenresults.xhtml"
  let requestHeaders = [
          ("Host","www.tinglysning.dk"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Accept-Encoding", "gzip, deflate"),
          ("Referer", "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"),
          ("Connection", "keep-alive"),
          ("Pragma", "no-cache"),
          ("Cache-Control", "no-cache")]
  response <- try $ doGetRequest url requestHeaders _afPfm viewState cookie :: IO (Either SomeException (String, [Cookie]))
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ Just response 


doTrdRequest vin _afPfm viewState cookie = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  let requestHeaders = [
          ("Host","www.tinglysning.dk"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Accept-Encoding", "gzip, deflate"),
          ("Referer", B.pack(url)),
          ("Connection", "keep-alive"),
          ("Pragma", "no-cache"),
          ("Cache-Control", "no-cache")]
  let body = [
        ("soegemaade", "content:center:bilbogen:stelnrOption"),
        ("content:center:bilbogen:stelnr", B.pack(vin)),
        ("content:center:bilbogen:cvr", ""),
        ("content:center:bilbogen:navn", ""),
        ("content:center:bilbogen:foedselsdato", ""),
        ("bogsattest", "content:center:bilbogen:uofficiel"),
        ("org.apache.myfaces.trinidad.faces.FORM", "j_id4"),
        ("_noJavaScript", "false"),
        ("javax.faces.ViewState", B.pack(viewState)),
        ("source","content:center:bilbogen:stelnrOption")]
  response <- try $ doPostRequest url requestHeaders body _afPfm cookie :: IO (Either SomeException (String, [Cookie]))
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ Just response


doSndRequest vin _afPfm viewState cookie = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  let requestHeaders = [
          ("Host","www.tinglysning.dk"),
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0"),
          ("Accept", "text/html,application/xhtml+xml;q=0.9,*/*;q=0.8"),
          ("Accept-Language", "en-GB,en;q=0.5"),
          ("Accept-Encoding", "gzip, deflate"),
          ("Referer", B.pack(url)),
          ("Tr-XHR-Message","true"),
          ("Connection", "keep-alive"),
          ("Pragma", "no-cache"),
          ("Cache-Control", "no-cache")]
  let body = [
        ("soegemaade", "content:center:bilbogen:stelnrOption"),
        ("content:center:bilbogen:stelnr", B.pack(vin)),
        ("content:center:bilbogen:cvr", ""),
        ("content:center:bilbogen:navn", ""),
        ("content:center:bilbogen:foedselsdato", ""),
        ("bogsattest", "content:center:bilbogen:uofficiel"),
        ("org.apache.myfaces.trinidad.faces.FORM", "j_id4"),
        ("_noJavaScript", "false"),
        ("javax.faces.ViewState", B.pack(viewState)),
        ("source","content:center:bilbogen:stelnrOption"),
        ("event","autosub"),
        ("partial","true")]
  response <- try $ doPostRequest url requestHeaders body _afPfm cookie :: IO (Either SomeException (String, [Cookie]))
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ procSndResponse response

doFstRequest :: IO (Maybe (String, String, [Cookie]))
doFstRequest = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  let requestHeaders = [
          ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0")]
  response <- try $ doSimplerGetRequest url requestHeaders :: IO (Either SomeException (String, [Cookie]))
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ procFstResponse response 

procFstResponse :: (String, [Cookie]) -> Maybe (String, String, [Cookie])
procFstResponse response = do
  a1 <- filterForm $ fst response
  _afPfm <- getParameterAt a1 0
  let cookieList = snd response
  viewState <- filterInput $ fst response
  return (_afPfm, viewState, cookieList)

procSndResponse :: (String, [Cookie]) -> Maybe (String, [Cookie])
procSndResponse response = do
  a1 <- filterContent $ fst response
  _afPfm <- getParameterAt a1 0
  let cookieList = snd response
  return (_afPfm, cookieList)

filterForm :: String -> Maybe String
filterForm a = case listToMaybe $ filter (isTagOpenName "form") (parseTags a) of
               Nothing -> Nothing
               Just result -> Just (fromAttrib "action" result)

filterInput :: String -> Maybe String
filterInput a = case listToMaybe $ filter (~== ("<input name=javax.faces.ViewState" :: String)) $ filter (isTagOpenName "input") (parseTags a) of
                 Nothing -> Nothing
                 Just result -> Just (fromAttrib "value" result)

filterContent :: String -> Maybe String
filterContent a = case listToMaybe $ filter (isTagOpenName "content") (parseTags a) of
               Nothing -> Nothing
               Just result -> Just (fromAttrib "action" result)

doRequests = do
  a1 <- doFstRequest
  case a1 of
   Nothing -> return Nothing
   Just a1 -> do
     let _afPfm = extractFst a1
     putStrLn _afPfm
     let viewState = extractSnd a1
     let cookie = extractTrd a1
     a2 <- doSndRequest "" _afPfm viewState cookie 
     case a2 of
      Nothing -> return Nothing
      Just a2 -> do
        let _afPfm2 = fst a2
        putStrLn _afPfm2
        let cookie = snd a2
        a3 <- doTrdRequest "WAUZZZ8P2AA090943" _afPfm2 viewState cookie
        a4 <- doFthRequest _afPfm2 viewState cookie
        return $ Just a2


past :: UTCTime
past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)

future :: UTCTime
future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)

newCookie :: B.ByteString -> B.ByteString -> Cookie
newCookie name value = Cookie { cookie_name = name
                 , cookie_value = value
                 , cookie_expiry_time = future
                 , cookie_domain = "www.tinglysning.dk"
                 , cookie_path = "/"
                 , cookie_creation_time = past
                 , cookie_last_access_time = past
                 , cookie_persistent = False
                 , cookie_host_only = True
                 , cookie_secure_only = False
                 , cookie_http_only = True
                 }

doSimplerGetRequest :: String -> RequestHeaders -> IO (String, [Cookie])
doSimplerGetRequest url requestHeadersList = do
  initReq <- parseUrl url
  let req = initReq {
        secure = False
        , method = "GET"
        , requestHeaders = requestHeadersList
        }
  resp <- withManager $ httpLbs req
  let cookieJar = responseCookieJar resp
  let cookieList = destroyCookieJar cookieJar
  return (LB.unpack $ responseBody resp, cookieList)

doGetRequest :: String -> RequestHeaders -> String -> String -> [Cookie] -> IO (String, [Cookie])
doGetRequest url requestHeadersList _afPfm viewState cookie = do
  initReq <- parseUrl $ url ++ "?" ++ _afPfm
  let req = initReq {
        secure = False
        , method = "GET"
        , cookieJar = Just $ createCookieJar cookie
        , requestHeaders = requestHeadersList
        }
  resp <- withManager $ httpLbs req
  let cookieJar = responseCookieJar resp
  let cookieList = destroyCookieJar cookieJar
  return (LB.unpack $ responseBody resp, cookieList)

doPostRequest :: String -> RequestHeaders -> [(B.ByteString, B.ByteString)] -> String -> [Cookie] -> IO (String, [Cookie])
doPostRequest url requestHeadersList body _afPfm cookie = do
  initReq <- parseUrl $ url ++ "?" ++ _afPfm
  let req' = initReq {
        secure = False
        , method = "POST"
        --, cookieJar = Just cookieJar
        , cookieJar = Just $ createCookieJar cookie
        --, cookieJar = Just $ createCookieJar [newcookie] 
        , requestHeaders = requestHeadersList
        }
  let req = urlEncodedBody body $ req'
  resp <- withManager $ httpLbs req
  let cookieJar = responseCookieJar resp
  let cookieList = destroyCookieJar cookieJar
  return (LB.unpack $ responseBody resp, cookieList)
  
