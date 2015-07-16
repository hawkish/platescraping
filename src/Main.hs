{-- Stuff supposed to be here --}

import Trafikstyrelsen (getVIN)
import Tinglysning (doRequests)
import LandRegisterTypes (LandRegister)
import qualified Data.Text as T
import Control.Monad.Trans

--main :: IO (Maybe LandRegister)
main = do
  putStrLn "Please input the registration plate number:"
  registrationnumber <- getLine
  vin <- liftIO $ getVIN $ T.pack registrationnumber
  case vin of
    Nothing -> putStrLn "Nothing found."
    Just vin -> do
      landRegister <- doRequests vin
      putStrLn $ show landRegister
    
