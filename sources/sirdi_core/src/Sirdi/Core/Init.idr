module Sirdi.Core.Init

import Sirdi.Core
import Sirdi.Core.Paths

import System


export
withStore : ((1 store : Store) -> IO a) -> IO a
withStore cont = do
    ignore $ system "mkdir -p \{show sirdiDir}"
    cont MkStore
