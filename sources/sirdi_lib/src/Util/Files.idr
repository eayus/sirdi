module Util.Files

import System.File.Process
import System.File.ReadWrite
import System

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

