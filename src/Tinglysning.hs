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

{--
getParameterAndCookie :: IO (Maybe (String, Cookie))
getParameterAndCookie = do
  response <- getHTMLTinglysning
  case response of
   Nothing -> return Nothing
   Just response -> do
     let parameter = getParameter $ fst response
     case parameter of
      Nothing -> return Nothing
      Just parameter -> do
        let cookie = snd response
        return $ Just (parameter, cookie)
--}

doSndRequest vin _afPfm viewState cookie = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  response <- try $ doPostRequest vin url _afPfm viewState cookie :: IO (Either SomeException (String, Cookie))
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     let html = fst response
     let cookie = snd response
     return $ Just (html, cookie)

doFstRequest :: IO (Maybe (String, String, [Cookie]))
doFstRequest = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  response <- try $ doGetRequest url :: IO (Either SomeException (String, [Cookie]))
  case response of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right response -> do
     return $ procFstResponse response 

procFstResponse :: (String, [Cookie]) -> Maybe (String, String, [Cookie])
procFstResponse response = do
  a1 <- filterAction $ fst response
  _afPfm <- getParameterAt a1 0
  let cookieList = snd response
  viewState <- filterInput $ fst response
  return (_afPfm, viewState, cookieList)

filterAction :: String -> Maybe String
filterAction a = case listToMaybe $ filter (isTagOpenName "form") (parseTags a) of
               Nothing -> Nothing
               Just result -> Just (fromAttrib "action" result)

filterInput :: String -> Maybe String
filterInput a = case listToMaybe $ filter (~== ("<input name=javax.faces.ViewState" :: String)) $ filter (isTagOpenName "input") (parseTags a) of
                 Nothing -> Nothing
                 Just result -> Just (fromAttrib "value" result)



doRequests = do
  a1 <- doFstRequest
  case a1 of
   Nothing -> return Nothing
   Just a1 -> do
     let _afPfm = extractFst a1
     putStr _afPfm
     let viewState = extractSnd a1
     let cookie = extractTrd a1
     a2 <- doSndRequest "" _afPfm viewState cookie
     return $ Just a2


doGetRequest :: String -> IO (String, [Cookie])
doGetRequest url = do
  initReq <- parseUrl url
  let req' = initReq { secure = True }
  let req = req' {
        requestHeaders = [("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0")]
        }
  resp <- withManager $ httpLbs req
  let cookieJar = responseCookieJar resp
  let cookieList = destroyCookieJar cookieJar
  return (LB.unpack $ responseBody resp, cookieList)

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


doPostRequest :: String -> String -> String -> String -> [Cookie] -> IO (String, Cookie)
doPostRequest vin url _afPfm viewState cookie = do
  --let cookie = destroyCookieJar cookieJar
  let name = cookie_name $ head cookie
  let value = cookie_value $ head cookie
  let newcookie = newCookie name value
  initReq <- parseUrl $ url ++ "?" ++ _afPfm
  let req' = initReq {
        secure = True
        , method = "POST"
        --, cookieJar = Just cookieJar
        , cookieJar = Just $ createCookieJar cookie
        --, cookieJar = Just $ createCookieJar [newcookie] 
        , requestHeaders = [
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
        }
  let req = urlEncodedBody [("soegemaade", "content:center:bilbogen:stelnrOption"),
                                ("content:center:bilbogen:stelnr", ""),
                                ("content:center:bilbogen:cvr", ""),
                                ("content:center:bilbogen:navn", ""),
                                ("content:center:bilbogen:foedselsdato", ""),
                                ("bogsattest", "content:center:bilbogen:uofficiel"),
                                ("org.apache.myfaces.trinidad.faces.FORM", "j_id4"),
                                ("_noJavaScript", "false"),
                                ("javax.faces.ViewState", B.pack(viewState)),
                                ("source","content:center:bilbogen:stelnrOption"),
                                ("event","autosub"),
                                ("partial","true")] $ req'
  
  resp <- withManager $ httpLbs req
  let cookieJar = responseCookieJar resp
  let cookie = head $ destroyCookieJar cookieJar
  return (LB.unpack $ responseBody resp, cookie)
 

