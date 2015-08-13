{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Utils (getElementAfter, getElementsAfter, getParameterAt, getElementAt, getTagStrings, getTagTexts) where

import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Text.HTML.TagSoup (parseTags, fromTagText, isTagText, isTagOpen, fromAttrib, Tag)

t = "<input type=\"submit\" name=\"ctl00$m$g_4156985a_4cd3_409b_aab5_4416025b40bb$ctl00$btnDefaultSend\" value=\"\" onclick=\"javascript:WebForm_DoPostBackWithOptions(new WebForm_PostBackOptions(&quot;ctl00$m$g_4156985a_4cd3_409b_aab5_4416025b40bb$ctl00$btnDefaultSend&quot;, &quot;&quot;, true, &quot;&quot;, &quot;&quot;, false, false))\" id=\"ctl00_m_g_4156985a_4cd3_409b_aab5_4416025b40bb_ctl00_btnDefaultSend\" style=\"display: none\" />\r\n <table id=\"tblInspections\" class=\"reports responsive\">\r\n                        <thead>\r\n                            <tr>\r\n                                <th>Dato</th>\r\n                                <th>Resultat</th>\r\n                                <th>Km-stand</th>\r\n                                <th>Reg.nr.</th>\r\n                                <th class=\"hideOnSmall\">Gem/send</th>\r\n                            </tr>\r\n                        </thead>\r\n                        <tbody>\r\n                \r\n                    <tr class=\"odd\" onclick=\"location.href=&quot;/Sider/synsrapport.aspx?Inspection=8974365&amp;Vin=11M007467&quot;\">\r\n\t\t\t\t\t\t<td>12-05-2010</td>\r\n\t\t\t\t\t\t<td><span class=\"noLink infoIcon\" title='Godkendt'>GOD</span></td>\r\n\t\t\t\t\t\t<td>65.000</td>\r\n\t\t\t\t\t\t<td>EZ12647</td>\r\n\t\t\t\t\t\t<td class=\"hideOnSmall\">\r\n                            <a id=\"ctl00_m_g_4156985a_4cd3_409b_aab5_4416025b40bb_ctl00_rptInspections_ctl01_lnkSave\" class=\"saveIcon\" href=\"/_layouts/Web.Inspections/SaveReport.aspx?Inspection=8974365&amp;Vin=11M007467\"></a>\r\n                            <a id=\"ctl00_m_g_4156985a_4cd3_409b_aab5_4416025b40bb_ctl00_rptInspections_ctl01_lnkSend\" class=\"sendIcon send popup\" href=\"../_CONTROLTEMPLATES/Web.Inspections.WebParts/SearchResultWebPart/#8974365\"></a>\r\n                        </td>\r\n\t\t\t\t\t</tr>\r\n\t\t\t\t\t\r\n                \r\n                    <tr class=\"even\" onclick=\"location.href=&quot;/Sider/synsrapport.aspx?Inspection=509709&amp;Vin=11M007467&quot;\">\r\n\t\t\t\t\t\t<td>10-10-2005</td>\r\n\t\t\t\t\t\t<td><span class=\"noLink infoIcon\" title='Godkendt'>GOD</span></td>\r\n\t\t\t\t\t\t<td></td>\r\n\t\t\t\t\t\t<td></td>\r\n\t\t\t\t\t\t<td class=\"hideOnSmall\">\r\n                            <a id=\"ctl00_m_g_4156985a_4cd3_409b_aab5_4416025b40bb_ctl00_rptInspections_ctl02_lnkSave\" class=\"saveIcon\" href=\"/_layouts/Web.Inspections/SaveReport.aspx?Inspection=509709&amp;Vin=11M007467\"></a>\r\n                            <a id=\"ctl00_m_g_4156985a_4cd3_409b_aab5_4416025b40bb_ctl00_rptInspections_ctl02_lnkSend\" class=\"sendIcon send popup\" href=\"../_CONTROLTEMPLATES/Web.Inspections.WebParts/SearchResultWebPart/#509709\"></a>\r\n                        </td>\r\n\t\t\t\t\t</tr>\r\n\t\t\t\t\t\r\n                \r\n                        </tbody>\r\n                    </table>"

h = T.pack t

getLocationHref a = filter (elemT 'W') a 

elemT c str = isJust (T.findIndex (== c) str)

getOnClick :: T.Text -> [T.Text]
getOnClick a = dequote $ map (fromAttrib "onclick") (getOpenTags a)

getOpenTags :: T.Text -> [Tag T.Text]
getOpenTags a = filter isTagOpen (parseTags a)

dequote :: [T.Text] -> [T.Text]
dequote = filter (not . T.null) 


getElementAfter :: Eq a => a -> [a] -> Maybe a
getElementAfter a b = do
  index <- elemIndex a b
  result <- getElementAfter' index b 
  return result

getElementAfter' :: Int -> [b] -> Maybe b
getElementAfter' index list = do
  result <- listToMaybe $ drop (index+1) list
  return result

getElementsAfter :: (Eq a) => a -> [a] -> [Maybe a]
getElementsAfter a b = do
  let indices = elemIndices a b
  let result = getElementsAfter' indices b
  getElementsAfter' indices b

getElementsAfter' :: [Int] -> [a] -> [Maybe a]
getElementsAfter' [] list = []
getElementsAfter' (x:xs) list = listToMaybe (drop (x+1) list) : getElementsAfter' xs list

-- Avoiding case expression ladder with Monad. 
getParameterAt :: T.Text -> Int -> Maybe T.Text
getParameterAt a n = do
  a1 <- getParametersAsString a
  a2 <- getParameterAt' a1 n
  return a2

getParametersAsString :: T.Text -> Maybe T.Text
getParametersAsString a = listToMaybe . drop 1 $ T.splitOn "?" a

getParameterAt' :: T.Text -> Int -> Maybe T.Text
getParameterAt' a n = getElementAt list n
                      where list = T.splitOn "&" a
                         
getElementAt :: [a] -> Int -> Maybe a
getElementAt a n = if n > length a - 1
                      then Nothing
                      else Just $ a !! n

getCookie :: String -> Maybe String
getCookie a = do
  a1 <- listToMaybe . drop 1 $ splitOn ";" a
  a2 <- listToMaybe $ splitOn "?" a1
  a3 <- listToMaybe . drop 1 $ splitOn "=" a2
  return a3
                                
-- Take all relevant tagTexts from the HTML tag soup. Yeah, it's a convoluted process...
getTagStrings :: String -> [String]
getTagStrings = map T.unpack . getTagTexts . T.pack

-- Take all relevant tagTexts from the HTML tag soup. Yeah, it's a convoluted process...
getTagTexts :: T.Text -> [T.Text]
getTagTexts = dequote . map f . filter isTagText . parseTags 
  where f = T.unwords . T.words . fromTagText
        dequote = filter (not . T.null)

