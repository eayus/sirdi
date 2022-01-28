module Sirdi.Core.Run

import Sirdi.Core
import Sirdi.Core.Init
import System
import System.Path


export
run : HasIO io => Initialised => (pkg : Package Built ident) -> io Int
run pkg = system $ show $ ((outputsDir /> pkg.identHash') /> "exec") /> "main"
