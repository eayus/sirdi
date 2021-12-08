module Config

import System.File.ReadWrite
import Language.JSON
import Data.List
import Util


public export
data Location
    = Link String
    | Local String

public export
record Config where
    constructor MkConfig
    deps : List Location
    modules : List (String) -- Need a better type for this one


parseConfig : String -> Maybe Config
parseConfig s = case parse s of
                     Just (JObject [("deps", JArray deps), ("modules", JArray mods)]) => Just $ MkConfig (catMaybes $ map parseLoc deps) (catMaybes $ map getStr mods)
                     _ => Nothing
      where
            getStr : JSON -> Maybe String
            getStr (JString s) = Just s
            getStr _ = Nothing


            parseLoc : JSON -> Maybe Location
            parseLoc (JObject [("link", x)]) = Link <$> getStr x
            parseLoc (JObject [("local", x)]) = Local <$> getStr x
            parseLoc _ = Nothing


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
