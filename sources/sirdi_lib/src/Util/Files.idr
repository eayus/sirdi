module Util.Files

import Data.Maybe

import System
import System.File.Process
import System.File.ReadWrite
import System.Path
import System.Info

readLinesOnto : HasIO io => (acc : List String) ->
                            (offset : Nat) ->
                            (fuel : Fuel) ->
                            (h : File) ->
                            io (Either FileError (Bool, List String))
readLinesOnto acc _ Dry h = pure (Right (False, reverse acc))
readLinesOnto acc offset (More fuel) h
  = do False <- fEOF h
         | True => pure $ Right (True, reverse acc)
       case offset of
            (S offset') => (fSeekLine h *> readLinesOnto acc offset' (More fuel) h) @{Applicative.Compose}
            0           => (fGetLine h >>= \str => readLinesOnto (str :: acc) 0 fuel h) @{Monad.Compose}


export
run : HasIO io => (cmd : String) -> io (Maybe String)
run cmd = do
    Right file <- popen cmd Read
        | Left err => pure Nothing

    Right (_, lines) <- readLinesOnto [] 0 forever file
        | Left err => pure Nothing

    pure (Just $ fastConcat lines)

isAbsolute' : Path -> Bool
isAbsolute' path =
  if isWindows then
    case path.volume of
      Just (UNC _ _) => True
      Just (Disk _) => path.hasRoot
      Nothing => False
  else
    path.hasRoot

export
appendPath : (left : Path) -> (right : Path) -> Path
appendPath left right =
  if isAbsolute' right || isJust right.volume then
    right
  else if hasRoot right then
    record { volume = left.volume } right
  else
    record { body = left.body ++ right.body, hasTrailSep = right.hasTrailSep } left
