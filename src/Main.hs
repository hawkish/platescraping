{-- Stuff supposed to be here --}

import Tinglysning (doRequests)
import LandRegisterTypes (LandRegister)

main :: IO (Maybe LandRegister)
main = doRequests
