module Package.Description

import Package.Identifier
import Package.Source
import Util
import Data.List


public export
record Description where
    constructor MkDescription
    deps : List (Identifier MaybePinned)
    modules : List String -- Need a better type for module names maybe?
    main : Maybe String
    executable : Maybe String
    passthru : List (String, String)


export
emptyDescription : Description
emptyDescription = MkDescription [] [] Nothing Nothing []


public export
MultiDescription : Type
MultiDescription = List (String, Description)


export
findSubDescription : String -> MultiDescription -> M Description
findSubDescription name multi =
    case lookup name multi of
         Just c => pure c
         Nothing => mErr "Cannot find definition for package \{name} in config"


export
getSubDescription : Maybe String -> MultiDescription -> M (String, Description)
getSubDescription (Just name) multi = do
    desc <- findSubDescription name multi
    pure (name, desc)
getSubDescription Nothing [ x ] = pure x
getSubDescription Nothing [] = mErr "Empty configuration"
getSubDescription Nothing _ = mErr "Need to specify which subconfig to build"
