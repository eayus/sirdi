module Util.All

import Data.List.Quantifiers
import Core.Context


export
mapM : Monad m => ((x : a) -> m (p x)) -> (xs : List a) -> m (All p xs)
mapM f []        = pure []
mapM f (x :: xs) = [| f x :: mapM f xs |]


export
mapAll : {0 xs : List a} -> ({0 x : a} -> p x -> b) -> All p xs -> List b
mapAll f [] = []
mapAll f (x :: xs) = f x :: mapAll f xs


export
traverseAll : Monad m => {0 xs : List a} -> ({0 x : a} -> p x -> m b) -> All p xs -> m (List b)
traverseAll f [] = pure []
traverseAll f (x :: xs) = [| f x :: traverseAll f xs |]


-- Specialise to Core since its not actually a Monad
export
traverseAll' : {0 xs : List a} -> ({0 x : a} -> p x -> Core b) -> All p xs -> Core (List b)
traverseAll' f [] = pure []
traverseAll' f (x :: xs) = [| f x :: traverseAll' f xs |]
