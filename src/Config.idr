module Config

import System.File.ReadWrite
import Language.JSON
import Data.List
import Util
import Data.Hashable


public export
data Location
    = Link String
    | Local String


export
hashLoc : Location -> String
hashLoc (Link s) = "dep\{show $ hash s}"
hashLoc (Local s) = "dep\{show $ hash s}"



public export
record Config where
    constructor MkConfig
    pkgName : String         -- Use a more precise type for this that captures valid package names
    deps : List Location
    modules : List String -- Need a better type for module names maybe?
    main : Maybe String
    passthru : List (String, String)


parseConfig : String -> Maybe Config
parseConfig s = case parse s of
                     Just (JObject obj) => do
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
                     _ => Nothing
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

            parseLoc : JSON -> Maybe Location
            parseLoc (JObject [("link", x)]) = Link <$> getStr x
            parseLoc (JObject [("local", x)]) = Local <$> getStr x
            parseLoc _ = Nothing

            parseMain : JSON -> Maybe String
            parseMain = getStr

            parseDeps : JSON -> Maybe (List Location)
            parseDeps (JArray deps) = sequence (map parseLoc deps)
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

export
readConfig : (dir : String) -> M Config
readConfig dir = do
    let filepath = "\{dir}/sirdi.json"

    Right contents <- mIO (readFile filepath) | Left err => mErr "Can't find file \{filepath}"

    case parseConfig contents of
         Just config => pure config
         Nothing => mErr "Failed to parse JSON."


public export
Eq Location where
    (Link x) == (Link y) = x == y
    (Local x) == (Local y) = x == y
    _ == _ = False


public export
Show Location where
    show (Link s) = "Link \{s}"
    show (Local s) = "Local \{s}"
