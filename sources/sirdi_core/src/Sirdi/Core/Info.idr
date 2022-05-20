module Sirdi.Core.Info

import Sirdi.Core
import Sirdi.Core.Paths
import Sirdi.Core.Config
import Sirdi.Core.Ipkg

import System.File.ReadWrite


data InfoError
    = InfoFileError FileError
    | InfoConfigError ConfigError


export
loadInfo : (0 store : Store)
        -> (ident : Identifier)
        -> Fetched store ident
        -> IO (Either InfoError (Info ident))
loadInfo _ ident _ = do

    Right contents <- readFile $ show $ cfgPath ident
        | Left err => pure $ Left $ InfoFileError err

    let Right config = parseConfig contents
        | Left err => pure $ Left $ InfoConfigError err

    mods <- findModules $ pkgDir ident /> "src"

    pure $ Right $ MkInfo config.depends config.main mods
