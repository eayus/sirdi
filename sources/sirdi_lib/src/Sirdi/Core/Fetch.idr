
module Sirdi.Core.Fetch

import Sirdi.Core
import Sirdi.Core.Init
import Sirdi.Core.Config
import Util.IOEither
import Language.TOML
import Data.Hashable
import System.Path
import System
import System.Directory
import System.Directory.Tree
import System.File.ReadWrite
import Util.Files
import Util.Git
import Idris.Package
import Compiler.Common
import Core.Core
import Core.Context.Context
import Core.Context
import Idris.Syntax
import Idris.REPL.Opts
import Idris.Package.Types


public export
data FetchError : Identifier -> Type where
    BadGitRepo : (badURL : String) -> FetchError (Git badURL commit path)
    BadGitCommit : (badCommit : String) -> FetchError (Git url badCommit path)
    BadGitPath : (badPath : Path) -> FetchError (Git url commit badPath)

    BadLocalPath : (badPath : Path) -> FetchError (Local badPath)


    NoConfigFile : String -> FetchError ident

    -- TOML file could not be parsed or has the wrong format
    BadTOML : TOMLError -> FetchError ident

    -- Neither TOML nor IPKG could be parsed correctly
    NoValidConfigFile : TOMLError -> Core.Error -> FetchError ident

||| Extract the content of a file that think is a config file
readConfigFile : (filename : String) -> IOEither (FetchError ident) String
readConfigFile filename =
    case !(readFile filename) of
        Left FileNotFound => throw (NoConfigFile filename)
        Left e => putStrLn "Error while reading config file" >> die e
        Right x => pure x

||| Convert a ipkg to a description, because we do not know where to find them
||| In principle the ipkg can have dependencies that are installed with the compiler
||| such as network and linear but others won't be installed by Sirdi
pkgToDesc : PkgDesc -> Description
pkgToDesc pkg = MkDescription (snd <$> mainmod pkg) []

||| Attempt to parse the given file as an ipkg
parseipkg : String -> IOEither Core.Error Description
parseipkg path = do

    -- We need to Generate the global state in which `parsePkgFile` operates
    defs <- coreToIOEither initDefs

    r1 <- MkRef <$> newIORef defs
    syn  <- MkRef <$> newIORef initSyntax
    opts <- MkRef <$> newIORef (defaultOpts Nothing (REPL NoneLvl) [])

    -- We parse the file for real
    let p = parsePkgFile {c = r1} {s = syn} {o = opts} path
    parsed <- coreToIOEither p

    pure (pkgToDesc parsed)

configFilePackage : Path -> String -> IOEither (FetchError ident) (Package Fetched ident)
configFilePackage path identHash = do
    let cfgFile = path /> configName

    contents <- readConfigFile (show cfgFile)

    desc <- mapErr BadTOML $ fromEither $ parseDesc contents

    pure $ MkPackage { desc = desc, identHash = identHash }


||| Copy a local dependency to the source directory and extract the TOML information from it
export
fetch : Initialised => (ident : Identifier) -> IOEither (FetchError ident) (Package Fetched ident)
fetch (Local path) = do

    let identHash = "local" ++ (show $ hash $ show path)
    let pkgDir = sirdiDir /> identHash
    let destDir = pkgDir /> "src"

    unless !(exists $ show pkgDir) (dieOnLeft $ createDir $ show pkgDir)

    ignore $ system "rm -rf \{show destDir}"

    ignore $ system "cp -r \{show path}/src \{show destDir}"

    configFilePackage path identHash


fetch (Git url commit path) = do
    ?git
  {-
    -- The hash that identifies this package
    let identHash = show $ hash "git-\{commit}"
    -- The destination folder where all the sources live
    let destDir = sourcesDir /> identHash

    Just current <- map parse <$> currentDir
      | Nothing => die "could not get current dir"
    putStrLn "currentDir : \{show current}"
    -- clone to a temporary repo
    removeDir "/tmp/sirdi"
    unless !(exists "/tmp/sirdi")
           (dieOnLeft $ createDir "/tmp/sirdi")
    ignore $ changeDir "/tmp/sirdi"

    Git.clone url (Just identHash)
    ignore $ changeDir "/tmp/sirdi/\{identHash}"
    Git.checkout commit

    -- copy the temp directory into the sources directory
    ignore $ changeDir (show current)
    ignore $ copyDir (parse "/tmp/sirdi/\{identHash}" `appendPath` path) destDir
    ignore $ removeDir "/tmp/sirdi/\{identHash}"

    -- find and read the TOML file
    configFilePackage path identHash
    -}
