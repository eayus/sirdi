module Sirdi.Core.Init

import Sirdi.Core
import Util.IOEither
import System.Directory
import System.Path


public export
data InitError : Type where
    NoConfigFile : InitError


export
Initialised : Type
Initialised = ()


export
init : IOEither InitError Initialised
init = do
    unless !(exists configName) (throw NoConfigFile)

    dieOnLeft $ createDir $ show sourcesDir
    dieOnLeft $ createDir $ show outputsDir

