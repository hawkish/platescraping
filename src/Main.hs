{-- Stuff supposed to be here --}

import Trafikstyrelsen (getVIN)
import Tinglysning (doRequests)
import LandRegisterTypes (LandRegister)
import qualified Data.Text as T
import Control.Monad.Trans

--main :: IO (Maybe LandRegister)
main = do
  --vin <- liftIO $ getVIN $ T.pack "AF22454"
  vin <- liftIO $ getVIN $ T.pack "AB12345"
  case vin of
    Nothing -> return Nothing
    Just vin -> doRequests vin
