{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
module LandRegisterTypes (initCreditor, initDebtor, initMotorregister, initDocument, initAdditionalText, initLandRegister, Creditor, Debtor, Motorregister, Document, AdditionalText, LandRegister) where

import Text.HTML.TagSoup (parseTags, Tag, Tag(..), (~==), (~/=), sections, fromTagText, fromAttrib, isTagText, isTagOpenName, isTagOpen)
import Utils (getElementAfter, getElementsAfter, getElementAt, getTagTexts)
import Data.Maybe
import Data.List.Split
import Data.List
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import Control.Lens




data Motorregister = MkMotorregister { _brand :: Maybe T.Text
                                     , _year :: Maybe T.Text
                                     , _license :: Maybe T.Text
                                     } deriving (Eq, Show, Read)

makeLenses ''Motorregister

data Document = MkDocument { _date :: Maybe T.Text
                           , _mortgage :: Maybe T.Text
                           , _documentType :: Maybe T.Text
                           , _principal :: Maybe T.Text
                           , _rateOfInterest :: Maybe T.Text
                           } deriving (Eq, Show, Read)

makeLenses ''Document

data Creditor = MkCreditor { _cname :: Maybe T.Text
                           , _cvr :: Maybe T.Text
                           } deriving (Eq, Show, Read)

makeLenses ''Creditor

data Debtor = MkDebtor { _dname :: Maybe T.Text
                       , _cpr :: Maybe T.Text
                       } deriving (Eq, Show, Read)

makeLenses ''Debtor

data AdditionalText = MkAdditionalText { _text :: [Maybe T.Text] } deriving (Eq, Show, Read)

makeLenses ''AdditionalText

data LandRegister = MkLandRegister { _motorregister :: Motorregister
                                   , _document :: Document
                                   , _creditor :: Creditor
                                   , _debtor :: Debtor
                                   , _additionalText :: AdditionalText
                                   } deriving (Eq, Show, Read)

makeLenses ''LandRegister

getTextAfter :: T.Text -> T.Text -> Maybe T.Text
getTextAfter a b = getElementAfter a $ getTagTexts b

getTextsAfter :: T.Text -> T.Text -> [Maybe T.Text]
getTextsAfter a b = getElementsAfter a $ getTagTexts b

initCreditor a = MkCreditor { _cname = getCreditorName a, _cvr = getCreditorName a }

initDebtor a = MkDebtor { _dname = getDebtorName a, _cpr = getDebtorCPR a }

initDocument a = MkDocument { _date = getDocumentDate a, _mortgage = getDocumentMortgage a, _documentType = getDocumentDocumentType a, _principal = getDocumentPrincipal a, _rateOfInterest = getDocumentRateOfInterest a }

initMotorregister a = MkMotorregister { _brand = getMotorregisterBrand a, _year = getMotorregisterYear a, _license = getMotorregisterLicense a }

initAdditionalText a = MkAdditionalText { _text = getAdditionalText a }

initLandRegister a = MkLandRegister { _motorregister = initMotorregister a, _document = initDocument a, _creditor = initCreditor a, _debtor = initDebtor a, _additionalText = initAdditionalText a }

--setCreditor c a = c & (cname .~ (getCreditorName a)) . (cvr .~ (getCreditorCVR a))

--getCreditor c = c ^. cname
  
getMotorregisterBrand :: T.Text -> Maybe T.Text
getMotorregisterBrand a = getTextAfter (T.pack "Mærke:") a

getMotorregisterYear :: T.Text -> Maybe T.Text
getMotorregisterYear a = getTextAfter (T.pack "Årgang:") a

getMotorregisterLicense :: T.Text -> Maybe T.Text
getMotorregisterLicense a = getTextAfter (T.pack "Registreringsnummer:") a

getDocumentDate :: T.Text -> Maybe T.Text
getDocumentDate a = getTextAfter (T.pack "Dato/løbenummer:") a

getDocumentDocumentType :: T.Text -> Maybe T.Text
getDocumentDocumentType a = getTextAfter (T.pack "Dokument type:") a

getDocumentMortgage :: T.Text -> Maybe T.Text
getDocumentMortgage a = getTextAfter (T.pack "Prioritet:") a

getDocumentPrincipal :: T.Text -> Maybe T.Text
getDocumentPrincipal a = getTextAfter (T.pack "Hovedstol:") a

getDocumentRateOfInterest :: T.Text -> Maybe T.Text
getDocumentRateOfInterest a = getTextAfter (T.pack "Rentesats:") a

getDebtorName :: T.Text -> Maybe T.Text
getDebtorName a = do
  let names = getTextsAfter (T.pack "Navn:") a
  name <- getElementAt names 1
  return $ fromJust name

getDebtorCPR :: T.Text -> Maybe T.Text
getDebtorCPR a = getTextAfter (T.pack "CPR:") a

getCreditorName :: T.Text -> Maybe T.Text
getCreditorName a = do
  let names = getTextsAfter (T.pack "Navn:") a
  name <- getElementAt names 0
  return $ fromJust name

getCreditorCVR :: T.Text -> Maybe T.Text
getCreditorCVR a = getTextAfter (T.pack "CVR:") a

getAdditionalText :: T.Text -> [Maybe T.Text]
getAdditionalText = getAdditionalText' . getTagTexts 

getAdditionalText' :: [T.Text] -> [Maybe T.Text]
getAdditionalText' [] = []
getAdditionalText' (x:xs) | "Advarsel:" `T.isInfixOf` x = Just x : getAdditionalText' xs
                          | otherwise = getAdditionalText' xs
