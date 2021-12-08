module Util


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
mIO : IO a -> M a
mIO = MkM . map Right

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
