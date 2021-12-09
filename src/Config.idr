module Config

import System.File.ReadWrite
import Language.JSON
import Data.List
import Util
import Data.Hashable


public export
URL, FilePath : Type
URL = String
FilePath = String


public export
data Source
    = Git URL
    | Local FilePath


public export
record Dependency where
    constructor MkDep
    name : String
    source : Source


hashSource : Source -> String
hashSource (Git url) = show $ hash url
hashSource (Local fp) = show $ hash fp


export
depID : Dependency -> String
depID dep = "\{dep.name}\{hashSource dep.source}"


public export
record Config where
    constructor MkConfig
    pkgName : String         -- Use a more precise type for this that captures valid package names
    deps : List Dependency
    modules : List String -- Need a better type for module names maybe?
    main : Maybe String
    passthru : List (String, String)


public export
MultiConfig : Type
MultiConfig = List Config


pConfig : String -> Maybe MultiConfig
pConfig s = parse s >>= parseMultiConfig
      where
            getStr : JSON -> Maybe String
            getStr (JString s) = Just s
            getStr _ = Nothing

            parseName : JSON -> Maybe String
            parseName json = do
                s <- getStr json
                if length s > 0 && all isAlpha (unpack s)
                   then Just s
                   else Nothing

            parseSource : (String, JSON) -> Maybe Source
            parseSource ("git", x) = Git <$> getStr x
            parseSource ("local", x) = Local <$> getStr x
            parseSource _ = Nothing

            parseDep : JSON -> Maybe Dependency
            parseDep (JObject [("name", name), source]) = MkDep <$> getStr name <*> parseSource source
            parseDep _ = Nothing

            parseMain : JSON -> Maybe String
            parseMain = getStr

            parseDeps : JSON -> Maybe (List Dependency)
            parseDeps (JArray deps) = sequence (map parseDep deps)
            parseDeps _ = Nothing

            parseMods : JSON -> Maybe (List String)
            parseMods (JArray mods) = sequence (map getStr mods)
            parseMods _ = Nothing

            parsePassthru : JSON -> Maybe (List (String, String))
            parsePassthru (JObject p) = traverse go p
                where
                  go : (String, JSON) -> Maybe (String, String)
                  go (name, JString s) = Just (name, s)
                  go _ = Nothing
            parsePassthru _ = Nothing


            parseConfig : JSON -> Maybe Config
            parseConfig (JObject obj) = do
                pkgName <- lookup "name" obj >>= parseName
                deps <- lookup "deps" obj >>= parseDeps
                mods <- lookup "modules" obj >>= parseMods

                -- This looks strange but the crucial part is that if the "main" key
                -- is present, then we should fail if it can't parse the corresponding
                -- JSON. Equally, if the key isn't present, then that should be fine too.
                let main = parseMain <$> lookup "main" obj
                main <- sequence main

                let pthru = catMaybes . sequence $
                    (lookup "passthru" obj >>= parsePassthru)

                Just $ MkConfig pkgName deps mods main pthru
            parseConfig _ = Nothing

            parseMultiConfig : JSON -> Maybe MultiConfig
            parseMultiConfig (JArray configs) = traverse parseConfig configs
            parseMultiConfig _ = Nothing

export
readConfig : (dir : String) -> M MultiConfig
readConfig dir = do
    let filepath = "\{dir}/sirdi.json"

    Right contents <- mIO (readFile filepath) | Left err => mErr "Can't find file \{filepath}"

    case pConfig contents of
         Just config => pure config
         Nothing => mErr "Failed to parse JSON."


export
findSubConfig : String -> MultiConfig -> M Config
findSubConfig name multi =
    case find (\cfg => cfg.pkgName == name) multi of
         Just c => pure c
         Nothing => mErr "Cannot find definition for package \{name} in config"
