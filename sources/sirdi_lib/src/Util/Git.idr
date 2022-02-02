module Util.Git

import System
import Data.Maybe

export
clone : HasIO io => String -> Maybe String -> io ()
clone repo target = ignore $ system "git clone \{repo} \{fromMaybe "" target}"

export
checkout : HasIO io => String -> io ()
checkout hashLike = ignore $ system "git checkout \{hashLike}"
