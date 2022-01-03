module Sirdi.Core.Fetch

import Sirdi.Core
import Sirdi.Core.Init
import Sirdi.Core.Config
import Util.IOEither
import Language.TOML
import Data.Hashable
import System.Path
import System
import System.File.ReadWrite


public export
data FetchError : Identifier -> Type where
    BadGitRepo : (badURL : String) -> FetchError (Git badURL commit path)
    BadGitCommit : (badCommit : String) -> FetchError (Git url badCommit path)
    BadGitPath : (badPath : String) -> FetchError (Git url commit badPath)

    BadLocalPath : (badPath : String) -> FetchError (Local badPath)

    NoConfigFile : FetchError ident
    BadConfig : ConfigError -> FetchError ident


export
fetch : Initialised => (ident : Identifier) -> IOEither (FetchError ident) (Package Fetched ident)
fetch (Local path) = do
    let identHash = show $ hash "local-\{show path}"
    let destDir = outputsDir /> identHash

    ignore $ system "cp \{show path} \{show destDir}/"

    let cfgFile = destDir /> configName

    contents <- case !(readFile $ show cfgFile) of
        Left FileNotFound => throw NoConfigFile
        Left e => die e
        Right x => pure x

    desc <- MkEitherT $ pure $ bimap BadConfig id $ parseDesc contents

    pure $ MkPackage { desc = desc, identHash = identHash }

fetch (Git url commit path) = do
    putStrLn "GIT UNIMPLEMENTED!"

    ?doFetch_rhs_1
