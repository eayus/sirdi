module Util

import System
import Data.List

public export
URL, FilePath : Type
URL = String
FilePath = String


export
record M a where
  constructor MkM
  inner : IO (Either String a)


export
Functor M where
    map f (MkM x) = MkM (map (map f) x)


export
Applicative M where
    pure x = MkM (pure (Right x))

    MkM x <*> MkM y = MkM $ (map (<*>) x) <*> y


export
Monad M where
    MkM x >>= f = MkM $ do
      x' <- x
      case x' of
           Left err => pure $ Left err
           Right good => (f good).inner


export
HasIO M where
    liftIO = MkM . map Right

export
mErr : String -> M a
mErr s = MkM $ pure $ Left s


export
runM : M a -> IO (Maybe a)
runM m = do
    res <- m.inner
    case res of
         Left err => putStrLn "ERROR: \{err}" >> pure Nothing
         Right x => pure (Just x)


export
mSystem : (command : String) -> (onErr : String) -> M ()
mSystem command onErr = do
    n <- system command
    case n of
         0 => pure ()
         _ => mErr onErr


export
mRun : (command : String) -> M String
mRun command = do
    (out, n) <- run command
    case n of
         0 => pure out
         _ => mErr "Running \{show command} failed with error \{show out}"

export
nubOn : Eq b => (a -> b) -> List a -> List a
nubOn f a = nubBy ((==) `on` f) a


export
gitRemoteLatestCommit : URL -> M String
gitRemoteLatestCommit url = mRun "git ls-remote \{url} main | awk '{print $1}'"
