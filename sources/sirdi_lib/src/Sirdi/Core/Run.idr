module Sirdi.Core.Run

import Sirdi.Core
import Sirdi.Core.Init
import System
import System.Path
import Util.IOEither


public export
data RunError : Package Built ident -> Type where
    NoMainSpecified : {auto 0 noMain : pkg.description.main = Nothing} -> RunError pkg


export
run : Initialised => (pkg : Package Built ident) -> IOEither (RunError pkg) Int
run pkg with (pkg.description.main) proof p
  _ | Nothing = throw NoMainSpecified
  _ | Just _  = system $ show $ ((outputsDir /> pkg.identHash') /> "exec") /> "main"
