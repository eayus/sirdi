module Sirdi.Core.New

import Sirdi.Core
import System.Directory
import System.File.ReadWrite
import Util.IOEither


defaultDesc : String
defaultDesc = """
    main = "Main"
    dependencies = []
    """


defaultMain : String
defaultMain = """
    module Main

    main : IO ()
    main = putStrLn "Hello, world!"
    """


public export
data NewProjectError : Type where
    CreateDirError : String -> NewProjectError


export
new : (name : String) -> IOEither NewProjectError ()
new name = do
    -- TODO: Error handling could be a lot better, should first check if directory
    -- exists etc.

    Right () <- createDir name
        | Left err => throw $ CreateDirError $ show err

    dieOnLeft $ createDir $ "\{name}/src"

    dieOnLeft $ writeFile "\{name}/sirdi.toml" defaultDesc
    dieOnLeft $ writeFile "\{name}/src/Main.idr" defaultMain
