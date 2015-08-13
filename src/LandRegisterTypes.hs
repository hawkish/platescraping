{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveGeneric #-}
module LandRegisterTypes (initCreditor, initDebtor, initMotorregister, initDocument, initAdditionalText, initLandRegister, Creditor, Debtor, Motorregister, Document, AdditionalText, LandRegister) where

import Utils (getElementAfter, getElementsAfter, getElementAt, getTagTexts)
import Data.Maybe
import qualified Data.Text    as T
import Control.Lens
import GHC.Generics

data Motorregister = MkMotorregister { _brand :: Maybe T.Text
                                     , _year :: Maybe T.Text
                                     , _license :: Maybe T.Text
                                     , _vin :: Maybe T.Text
                                     } deriving (Eq, Show, Read, Generic)

makeLenses ''Motorregister

data Document = MkDocument { _date :: Maybe T.Text
                           , _mortgage :: Maybe T.Text
                           , _documentType :: Maybe T.Text
                           , _principal :: Maybe T.Text
                           , _rateOfInterest :: Maybe T.Text
                           } deriving (Eq, Show, Read, Generic)

makeLenses ''Document

data Creditor = MkCreditor { _cname :: Maybe T.Text
                           , _cvr :: Maybe T.Text
                           } deriving (Eq, Show, Read, Generic)

makeLenses ''Creditor

data Debtor = MkDebtor { _dname :: Maybe T.Text
                       , _cpr :: Maybe T.Text
                       } deriving (Eq, Show, Read, Generic)

makeLenses ''Debtor

data AdditionalText = MkAdditionalText { _text :: [Maybe T.Text] } deriving (Eq, Show, Read, Generic)

makeLenses ''AdditionalText

data LandRegister = MkLandRegister { _motorregister :: Motorregister
                                   , _document :: Document
                                   , _creditor :: Creditor
                                   , _debtor :: Debtor
                                   , _additionalText :: AdditionalText
                                   } deriving (Eq, Show, Read, Generic)

makeLenses ''LandRegister

data Surveyor = MkSurveyor { _surveyorName
                           , _cvr
                           , _place
                           } deriving (Eq, Show, Read, Generic)

makeLenses ''Surveyor

data Vehicle = MkVehicle { _brand :: Maybe T.Text
                         ,  _model :: Maybe T.Text
                         ,  _vehiclekind :: Maybe T.Text
                         ,  _registrationNumber :: Maybe T.Text
                         ,  _vin :: Maybe T.Text
                         ,  _vehicleID :: Maybe T.Text
                         } deriving (Eq, Show, Read, Generic)

makeLenses ''Vehicle

data SurveyorDetails = MkSurveyorDetails { _surveyorKind :: Maybe T.Text
                                         , _surveyorType :: Maybe T.Text
                                         , _surveyorDate :: Maybe T.Text
                                         , _endTime :: Maybe T.Text
                                         , _odometer :: Maybe T.Text
                                         , _surveyorResult :: Maybe T.Text
                                         , _surveyorDeadline :: Maybe T.Text
                                         } deriving (Eq, Show, Read, Generic)

makeLenses ''SurveyorDetails

data ErrorOverview = MkErrorOverview { _errorText :: Maybe T.Text } deriving (Eq, Show, Read, Generic)

makeLenses ''ErrorOverview

data ServiceRemarks = MkServiceRemarks {_serviceRemarks :: Maybe T.Text } deriving (Eq, Show, Read, Generic)

makeLenses ''ServiceRemarks

data SurveyorRapport = MkSurveyorRapport { _surveyor :: Surveyor
                                         , _vehicle :: Vehicle
                                         , _surveyorDetails :: SurveyorDetails
                                         , _errorOverview :: ErrorOverview
                                         , _systemRemarks :: ServiceRemarks
                                         } deriving (Eq, Show, Read, Generic)

makeLenses ''SurveyorRapport

data SurveyorRapports = MkSurveyorRapports { _surveyorRapports :: [SurveyorRapport] } deriving (Eq, Show, Read, Generic)

makeLenses ''SurveyorRapports

getTextAfter :: T.Text -> T.Text -> Maybe T.Text
getTextAfter a b = getElementAfter a $ getTagTexts b

getTextsAfter :: T.Text -> T.Text -> [Maybe T.Text]
getTextsAfter a b = getElementsAfter a $ getTagTexts b

initCreditor a = MkCreditor { _cname = getCreditorName a, _cvr = getCreditorName a }

initDebtor a = MkDebtor { _dname = getDebtorName a, _cpr = getDebtorCPR a }

initDocument a = MkDocument { _date = getDocumentDate a, _mortgage = getDocumentMortgage a, _documentType = getDocumentDocumentType a, _principal = getDocumentPrincipal a, _rateOfInterest = getDocumentRateOfInterest a }

initMotorregister vin a = MkMotorregister { _brand = getMotorregisterBrand a, _year = getMotorregisterYear a, _license = getMotorregisterLicense a, _vin = vin }

initAdditionalText a = MkAdditionalText { _text = getAdditionalText a }

initLandRegister vin a = MkLandRegister { _motorregister = initMotorregister vin a, _document = initDocument a, _creditor = initCreditor a, _debtor = initDebtor a, _additionalText = initAdditionalText a }

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
