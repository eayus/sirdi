
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

readConfigFile : String -> IOEither (FetchError ident) String
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

export
fetch : Initialised => (ident : Identifier) -> IOEither (FetchError ident) (Package Fetched ident)
fetch (Local path) = do
    let identHash = show $ hash "local-\{show path}"
    let destDir = sourcesDir /> identHash


    let cfgFile = path /> configName

    contents <- readConfigFile (show cfgFile)

    ignore $ system "cp -r \{show path}/src \{show destDir}/"

    desc <- MkEitherT $ pure $ bimap BadTOML id $ parseDesc contents

    pure $ MkPackage { desc = desc, identHash = identHash }


fetch (Git url commit path) = do
    let identHash = show $ hash "git-\{commit}"
    Just current <- map parse <$> currentDir
      | Nothing => die "could not get current dir"
    putStrLn "currentDir : \{current}"
    -- clone to a temporary repo
    removeDir "/tmp/sirdi"
    unless !(exists "/tmp/sirdi")
           (dieOnLeft $ createDir "/tmp/sirdi")
    ignore $ changeDir "/tmp/sirdi"

    Git.clone url (Just identHash)
    ignore $ changeDir "/tmp/sirdi/\{identHash}"
    Git.checkout commit

    -- copy the temp directory into the current director
    ignore $ changeDir (show current)
    ignore $ copyDir (parse "/tmp/sirdi/\{identHash}") (parse ".sirdi/sources")
    ignore $ removeDir "/tmp/sirdi/\{identHash}"

    -- making the package by creating a hash and parsing the config file
    -- if the config file cannot be parsed as a TOML file, the file is
    -- parsed as a ipkg file, note that ipkg files cannot have dependencies themselves

    ignore $ changeDir $ show (current /> identHash)
    printLn (current /> identHash)
    Just c <- currentDir
      | Nothing => die "could not get current dir"
    putStrLn "currentDir : \{c}"
    contents <- readConfigFile $ show (current /> (identHash </> path))
    ignore $ changeDir ".."

    -- Attempt to parse as TOML first, if that fails, attempt to parse as IPKG, if both
    -- fail, collect both errors in `NoValidConfigFile` so that we can report them
    desc <- either (\err => mapErr (NoValidConfigFile err) (parseipkg contents))
                   (MkEitherT . pure . Right)
                   (parseDesc contents)

    pure $ MkPackage { desc = desc, identHash = identHash }
