{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveGeneric #-}
module LandRegisterType (initCreditor, initDebtor, initMotorregister, initDocument, initAdditionalText, initLandRegister, Creditor, Debtor, Motorregister, Document, AdditionalText, LandRegister) where

import Utils (getElementAfter, getElementsAfter, getElementAt, getTagTexts, getTextAfter, getTextsAfter)
import Data.Maybe
import qualified Data.Text as T
import Control.Lens
import GHC.Generics

data LandRegister = MkLandRegister { _motorregister :: Motorregister
                                   , _document :: Document
                                   , _creditor :: Creditor
                                   , _debtor :: Debtor
                                   , _additionalText :: AdditionalText
                                   } deriving (Eq, Show, Read, Generic)

data Motorregister = MkMotorregister { _brand :: Maybe T.Text
                                     , _year :: Maybe T.Text
                                     , _license :: Maybe T.Text
                                     , _vin :: Maybe T.Text
                                     } deriving (Eq, Show, Read, Generic)

data Document = MkDocument { _date :: Maybe T.Text
                           , _mortgage :: Maybe T.Text
                           , _documentType :: Maybe T.Text
                           , _principal :: Maybe T.Text
                           , _rateOfInterest :: Maybe T.Text
                           } deriving (Eq, Show, Read, Generic)

data Creditor = MkCreditor { _cname :: Maybe T.Text
                           , _cvr :: Maybe T.Text
                           } deriving (Eq, Show, Read, Generic)

data Debtor = MkDebtor { _dname :: Maybe T.Text
                       , _cpr :: Maybe T.Text
                       } deriving (Eq, Show, Read, Generic)

data AdditionalText = MkAdditionalText { _text :: [Maybe T.Text] } deriving (Eq, Show, Read, Generic)

makeLenses ''LandRegister
makeLenses ''Motorregister
makeLenses ''Document
makeLenses ''Creditor
makeLenses ''Debtor
makeLenses ''AdditionalText

initLandRegister vin a = MkLandRegister { _motorregister = initMotorregister vin a, _document = initDocument a, _creditor = initCreditor a, _debtor = initDebtor a, _additionalText = initAdditionalText a }

initCreditor a = MkCreditor { _cname = getCreditorName a
                            , _cvr = getTextAfter (T.pack "CVR:") a }

getCreditorName :: T.Text -> Maybe T.Text
getCreditorName a = do
  let names = getTextsAfter (T.pack "Navn:") a
  name <- getElementAt names 0
  return $ fromJust name

initDebtor a = MkDebtor { _dname = getDebtorName a
                        , _cpr = getTextAfter (T.pack "CPR:") a }

getDebtorName :: T.Text -> Maybe T.Text
getDebtorName a = do
  let names = getTextsAfter (T.pack "Navn:") a
  name <- getElementAt names 1
  return $ fromJust name

initDocument a = MkDocument { _date = getTextAfter (T.pack "Dato/løbenummer:") a
                            , _mortgage = getTextAfter (T.pack "Prioritet:") a
                            , _documentType = getTextAfter (T.pack "Dokument type:") a
                            , _principal = getTextAfter (T.pack "Hovedstol:") a
                            , _rateOfInterest = getTextAfter (T.pack "Rentesats:") a }

initMotorregister vin a = MkMotorregister { _brand = getTextAfter (T.pack "Mærke:") a
                                          , _year = getTextAfter (T.pack "Årgang:") a
                                          , _license = getTextAfter (T.pack "Registreringsnummer:") a
                                          , _vin = vin }

initAdditionalText a = MkAdditionalText { _text = getAdditionalText a }

getAdditionalText :: T.Text -> [Maybe T.Text]
getAdditionalText = getAdditionalText' . getTagTexts 

getAdditionalText' :: [T.Text] -> [Maybe T.Text]
getAdditionalText' [] = []
getAdditionalText' (x:xs) | "Advarsel:" `T.isInfixOf` x = Just x : getAdditionalText' xs
                          | otherwise = getAdditionalText' xs
