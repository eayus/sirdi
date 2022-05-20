module Sirdi.Core.Init

import Sirdi.Core
import Util.IOEither
import System.Directory
import System.Path


public export
data InitError : Type where
    NoConfigFile : InitError


0 initFetched : Pred Identifier
0 initBuilt : Pred Identifier


export
initStore : (cont : forall fetched, built. (1 store : Store fetched built) -> IOEither err a)
         -> IOEither (Either InitError err) a
initStore cont = do
    unless !(exists configName) (throw $ Left NoConfigFile)

    unless !(exists $ show sirdiDir) (dieOnLeft $ createDir $ show sirdiDir)

    mapErr Right $ cont {fetched = initFetched, built = initBuilt} MkStore
