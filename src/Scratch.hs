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
