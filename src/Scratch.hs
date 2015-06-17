{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
module Scratch () where

import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, fromAttrib, isTagText, isTagOpenName, isTagOpen)
import Utils (getElementAt, getTagTexts)
import Data.Maybe
import Data.List.Split
import Data.List
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE

e = "<h4 class=\"header\">OPLYSNINGER FRA MOTORREGISTER. HVIS INGEN OPLYSNINGER, FINDES K\195\152RET\195\152JET IKKE I DMR:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">M\195\166rke:</td>\n<td class=\"right\">AUDI A3 2,0 TDI</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">\195\133rgang:</td>\n<td class=\"right\">2009</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Registreringsnummer:</td>\n<td class=\"right\">AM32511</td>\n</tr>\n</tbody>\n</table>\n<div class=\"linefeed\">&#160;</div>\n<hr class=\"long\"/>\n<h2 class=\"subheader\">H\195\134FTELSER</h2>\n<hr class=\"heading\"/>\n<h4 class=\"header\">DOKUMENT:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Dato/l\195\184benummer:</td>\n<td class=\"right\">13.05.2014-1005340550</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Prioritet:</td>\n<td class=\"right\">1</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Dokument type:</td>\n<td class=\"right\">Ejendomsforbehold</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Hovedstol:</td>\n<td class=\"right\">100.080 DKK</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Rentesats:</td>\n<td class=\"right\">3,95 %</td>\n</tr>\n</tbody>\n</table>\n<div class=\"linefeed\">&#160;</div>\n<div class=\"linefeed\">&#160;</div>\n<hr class=\"heading\"/>\n<h4 class=\"header\">KREDITORER:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Navn:</td>\n<td class=\"right\">NORDEA FINANS DANMARK A/S</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">CVR:</td>\n<td class=\"right\">89805910</td>\n</tr>\n</tbody>\n</table>\n<div class=\"linefeed\">&#160;</div>\n<hr class=\"heading\"/>\n<h4 class=\"header\">DEBITORER:</h4>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">Navn:</td>\n<td class=\"right\">Shahid Hussain Shah</td>\n</tr>\n</tbody>\n</table>\n<table>\n<tbody>\n<tr>\n<td class=\"left\">CPR:</td>\n<td class=\"right\">010460-****</td>o\n</tr>\n</tbody>\n</table>"

g = "λαθρανασκαφές στην περιοχή της Καρδίτσας"


f = TE.encodeUtf8 e

n = T.pack "<form id=\"j_id4\" name=\"j_id4\" style=\"margin:0px\" method=\"POST\" onkeypress=\"return _submitOnEnter(event,'j_id4');\" action=\"/tinglysning/forespoerg/bilbogen/bilbogen.xhtml;TDK_JSESSIONID=SlEuMoHouvqLGdJ_fICGH8lzaDjd6tNZ0WKFdKJ6vHtRHb7o3ZOj!1927900994!-478567563?_afPfm=-1bav6tyn4e\">\n\n&#9;<div class=\"container-logotop\">\n    <script type=\"text/javascript\" src=\"/tinglysning/js/helptext.js\"></script>\n    <script type=\"text/javascript\" src=\"/tinglysning/js/utils.js\"></script>\n"


a = T.pack "<input type=\"hidden\" name=\"_noJavaScript\" value=\"false\"><span id=\"tr_j_id4_Postscript\"><input type=\"hidden\" name=\"javax.faces.ViewState\" value=\"!-tcw82nrpf\"><input type=\"hidden\" name=\"source\">"


b = T.pack "<?xml version=\"1.0\" ?>\n<?Tr-XHR-Response-Type ?>\n<content action=\"/tinglysning/forespoerg/bilbogen/bilbogen.xhtml?_afPfm=-x8n0v5c5u\"> \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n<fragment>"

c = T.pack "<td class=\"af_column_cell-text OraTableBorder1111\"><span id=\"content:center:bilbogenresults:bilerid:0:stelnummer\">WAUZZZ8P2AA090943</span></td><td class=\"af_column_cell-text OraTableBorder1111\">AM32511</td><td class=\"af_column_cell-text OraTableBorder1111\">AUDI</td><td class=\"af_column_cell-text OraTableBorder1111\">2009</td><td class=\"af_column_cell-text OraTableBorder1111\"></td><td class=\"af_column_cell-text OraTableBorder1111\"><a id=\"content:center:bilbogenresults:bilerid:0:visbildetaljer\" name=\"content:center:bilbogenresults:bilerid:0:visbildetaljer\" onclick=\"submitForm('j_id4',1,{source:'content:center:bilbogenresults:bilerid:0:visbildetaljer','listItem':'f2922aa1-de72-4be6-8dc2-c57610a7c4ad'});return false;\" class=\"OraLink\" href=\"#\">Vis</a></td></tr></table></td></tr></table><script type=\"text/javascript\">_uixt_content_center_bilbogenresults_bilerid=new CollectionComponent('j_id4','content:center:bilbogenresults:bilerid');</script><input type=\"hidden\" name=\"content:center:bilbogenresults:bilerid:rangeStart\" value=\"0\"></div></div>\n\n  <p></p><input id=\"content:center:bilbogenresults:j_id118\" name=\"content:center:bilbogenresults:j_id118\" type=\"submit\" value=\"(S)&oslash;g igen\" onclick=\"submitForm('j_id4',1,{source:'content:center:bilbogenresults:j_id118'});return false;\" accesskey=\"S\">\n\n<br>\n<br>\n</td>"





filterForm :: T.Text -> Maybe T.Text
filterForm a = case listToMaybe $ filter (isTagOpenName "form") (parseTags a) of
               Nothing -> Nothing
               Just result -> Just (fromAttrib "action" result)


filterInputViewState :: T.Text -> Maybe T.Text
filterInputViewState a = case listToMaybe $ filter (~== ("<input name=javax.faces.ViewState" :: String)) $ filter (isTagOpenName "input") (parseTags a) of
                 Nothing -> Nothing
                 Just result -> Just (fromAttrib "value" result)

filterContent :: T.Text -> Maybe T.Text
filterContent a = case listToMaybe $ filter (isTagOpenName "content") (parseTags a) of
               Nothing -> Nothing
               Just result -> Just (fromAttrib "action" result)

filterAnchor :: T.Text -> Maybe T.Text
filterAnchor a = case listToMaybe $ filter (~== ("<a name=content:center:bilbogenresults:bilerid:0:visbildetaljer" :: String)) $ filter (isTagOpenName "a") (parseTags a) of
                 Nothing -> Nothing
                 Just result -> Just (fromAttrib "onclick" result)


filterInputRangeStart :: T.Text -> Maybe T.Text
filterInputRangeStart a = case listToMaybe $ filter (~== ("<input name=content:center:bilbogenresults:bilerid:rangeStart" :: String)) $ filter (isTagOpenName "input") (parseTags a) of
                 Nothing -> Nothing
                 Just result -> Just (fromAttrib "value" result)


getListItemValue :: T.Text -> Maybe T.Text
getListItemValue a = do
  a1 <- filterAnchor a
  let elem = T.splitOn "'" a1
  indexListItem <- elemIndex "listItem" elem
  let indexListItemValue = indexListItem + 2
  a2 <- getElementAt elem indexListItemValue
  return a2
