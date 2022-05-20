module Sirdi.Core.Run

import Sirdi.Core
import System
import System.Path
import Util.IOEither


public export
data RunError : Identifier -> Type where
    NoMainSpecified : {auto 0 desc : Description ident} -> {auto 0 noMain : desc.main = Nothing} -> RunError ident


export
run : (ident : Identifier) -> (desc : Description ident) -> (0 isBuilt : built ident) -> (0 store : Store built fetched) -> IOEither (RunError ident) Int
run ident desc _ _ with (desc.main) proof p
  _ | Nothing = throw NoMainSpecified
  _ | Just _  = system $ show $ (((sirdiDir /> ident.hash) /> "build") /> "exec") /> "main"
