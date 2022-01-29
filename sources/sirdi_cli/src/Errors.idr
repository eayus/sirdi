module Errors

import Sirdi
import System.Path
import Language.TOML
import Core.Core


public export
Show InitError where
    show NoConfigFile = "No config file in current directory found!"


public export
Show TOMLError where
    show (ParseError x) = "Parse Error: \{show x}"
    show (ValidateError x) = "Validation error: \{x}"


public export
Show (FetchError ident) where
    show (BadGitRepo badURL) = "Bad git repo \{badURL}"
    show (BadGitCommit badCommit) = "Bad git commit \{badCommit}"
    show (BadGitPath badPath) = "Bad git path \{show badPath}"
    show (BadLocalPath badPath) = "Bad local path \{show badPath}"
    show (NoConfigFile path) = "Config file \{path} not found"
    show (BadTOML x) = "Bad toml config \{show x}"
    show (NoValidConfigFile x y) = "Could not parse TOML: \{show x}\nFallback to IPKG failed: \{show y}"


public export
Show RecBuildError where
    show (FetchErr ident x) = "Fetch error:\n\{show x}"
    show (BuildErr (CompileError x)) = "Compilation error\n\{show x}"


public export
Show NewProjectError where
    show (CreateDirError x) = "Create dir error: \{x}"

