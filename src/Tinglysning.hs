{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, fromAttrib, isTagText, isTagOpenName, isTagOpen)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Control.Exception
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import Utils (firstMaybe, following, getParameterAt, getCookie)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.List
import Network
import Data.Time.Clock
import Data.Time.Calendar



n = "<form id=\"j_id4\" name=\"j_id4\" style=\"margin:0px\" method=\"POST\" onkeypress=\"return _submitOnEnter(event,'j_id4');\" action=\"/tinglysning/forespoerg/bilbogen/bilbogen.xhtml;TDK_JSESSIONID=SlEuMoHouvqLGdJ_fICGH8lzaDjd6tNZ0WKFdKJ6vHtRHb7o3ZOj!1927900994!-478567563?_afPfm=-1bav6tyn4e\">\n\n&#9;<div class=\"container-logotop\">\n    <script type=\"text/javascript\" src=\"/tinglysning/js/helptext.js\"></script>\n    <script type=\"text/javascript\" src=\"/tinglysning/js/utils.js\"></script>\n"

h = "/tinglysning/forespoerg/bilbogen/bilbogen.xhtml;TDK_JSESSIONID=E4gvjvzPV_6nECcCdeE8POCn_QtrIGoS0LU6---cO4zQBWaWw0bX!-478567563!1440987210?_afPfm=-11ykkgvvlm&some=234"

--soegemaade:content:center:bilbogen:stelnrOption
--content:center:bilbogen:stelnr:WAUZZZ8P2AA090943
--content:center:bilbogen:cvr:
--content:center:bilbogen:navn:
--content:center:bilbogen:foedselsdato:
--bogsattest:content:center:bilbogen:uofficiel
--org.apache.myfaces.trinidad.faces.FORM:j_id4
--_noJavaScript:false
--javax.faces.ViewState:!-pwa1crud3
--source:content:center:bilbogen:j_id150
-- "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
-- "https://www.tinglysning.dk/tinglysning/common/visdokument/visdokument.xhtml?_afPfm=1avzjdqz5s"

-- Get HTML from trafikstyrelsen.


getParameterAndCookie :: IO (Maybe (String, String))
getParameterAndCookie = do
  response <- getHTMLTinglysning
  case response of
   Nothing -> return Nothing
   Just html -> return $ getParameterAndCookie' html

filterAction :: String -> Maybe String
filterAction a = case firstMaybe $ filter (isTagOpenName "form") (parseTags a) of
               Nothing -> Nothing
               Just result -> Just (fromAttrib "action" result)

-- Avoiding case expression ladder with Maybe Monad. 
getParameterAndCookie' :: String -> Maybe (String, String)
getParameterAndCookie' a = do
   a1 <- filterAction a
   a2 <- getParameterAt a1 0
   a3 <- getCookie a1
   return (a2, a3)

getHTMLTinglysning :: IO (Maybe String)
getHTMLTinglysning = do
  let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml"
  html <- try $ doGetRequest url :: IO (Either SomeException String)
  case html of
   Left ex -> do
     putStrLn $ show ex
     return Nothing
   Right html -> return $ Just html

postFormAtTinglysning :: IO (Maybe String)
postFormAtTinglysning = do
  parameterAndCookie <- getParameterAndCookie
  case parameterAndCookie of
   Nothing -> return Nothing
   Just parameterAndCookie -> do
     let _afPfm = fst parameterAndCookie
     let cookie = snd parameterAndCookie
     let url = "https://www.tinglysning.dk/tinglysning/forespoerg/bilbogen/bilbogen.xhtml?" ++ _afPfm
     html <- try $ doPostRequest url cookie "WAUZZZ8P2AA090943" :: IO (Either SomeException String)
     case html of
      Left ex -> do
        putStrLn $ show ex
        return Nothing
      Right html -> return $ Just html
                      


doGetRequest :: String -> IO String
doGetRequest url = do
  initReq <- parseUrl url
  let req' = initReq { secure = True }
  let req = req' {
        requestHeaders = [("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0")]
        }
  resp <- withManager $ httpLbs req
  return $ L.unpack $ responseBody resp

past :: UTCTime
past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)

future :: UTCTime
future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)

cookie :: String -> Cookie
cookie value = Cookie { cookie_name = "TDK_JSESSIONID"
                 , cookie_value = B.pack(value)
                 , cookie_expiry_time = future
                 , cookie_domain = ""
                 , cookie_path = "/"
                 , cookie_creation_time = past
                 , cookie_last_access_time = past
                 , cookie_persistent = False
                 , cookie_host_only = False
                 , cookie_secure_only = False
                 , cookie_http_only = False
                 }
  
doPostRequest :: String -> String -> String -> IO String
doPostRequest url cookie_value vin = do
  initReq <- parseUrl url
  let req' = initReq { secure = True
                     , method = "POST"
                     , cookieJar = Just $ createCookieJar [cookie cookie_value] 
                     , requestHeaders = [("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:37.0) Gecko/20100101 Firefox/37.0")]}
  let req = urlEncodedBody [("soegemaade", "content:center:bilbogen:stelnrOption"),
                            ("content:center:bilbogen:stelnr", B.pack(vin)),
                            ("content:center:bilbogen:cvr", ""),
                            ("content:center:bilbogen:navn", ""),
                            ("content:center:bilbogen:foedselsdato", ""),
                            ("bogsattest", "content:center:bilbogen:uofficiel"),
                            ("org.apache.myfaces.trinidad.faces.FORM", "j_id4"),
                            ("_noJavaScript", "false"),
                            ("javax.faces.ViewState","!-pwa1crud3"),
                            ("source","content:center:bilbogen:j_id150")]
                             req'
  resp <- withManager $ httpLbs req
  return $ L.unpack $ responseBody resp
  

