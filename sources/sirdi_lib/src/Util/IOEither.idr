module Util.IOEither

import public Control.Monad.Either
import Core.Context
import System


public export
IOEither : Type -> Type -> Type
IOEither err = EitherT err IO


export
throw : e -> IOEither e a
throw = MkEitherT . pure . Left


export
mapErr : (e -> e') -> IOEither e a -> IOEither e' a
mapErr f (MkEitherT x) = MkEitherT $ bimap f id <$> x


export
embed : IO (Either e a) -> IOEither e a
embed = MkEitherT


export
die : HasIO io => Show e => e -> io a
die x = do print x; exitFailure


export
dieOnLeft : HasIO io => Show e => io (Either e a) -> io a
dieOnLeft f = case !(f) of
                   Left err => die err
                   Right x => pure x


export
coreToIOEither : Core a -> IOEither Error a
coreToIOEither x = MkEitherT $ coreRun x (pure . Left) (pure . Right)


export
runIOE : IOEither e a -> (e -> IO b) -> (a -> IO b) -> IO b
runIOE (MkEitherT x) f g = case !(x) of
                                Left err => f err
                                Right res => g res
