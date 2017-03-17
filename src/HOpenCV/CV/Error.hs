{- |
Module created for handling errors by monad.
-}
module HOpenCV.CV.Error
where

import Control.Monad.Error

data ErrorCV er a = Raise er | Return a deriving Show

instance (Error er) => Functor (ErrorCV er) where
        fmap = liftM

instance (Error er) => Applicative (ErrorCV er) where
        pure = return
        (<*>) = ap

instance (Error er) => Monad (ErrorCV er) where
        return x = Return x
        (Raise er) >>= f = Raise er
        (Return a) >>= f = f a
        fail msg = Raise (strMsg msg)

throwErrorCV :: er -> ErrorCV er a
throwErrorCV er = Raise er

type ErrCV a = ErrorCV String a

runErr_r :: ErrCV a -> a
runErr_r (Return a) = a

runErr_s :: ErrCV a -> String
runErr_s (Raise a) = a

-- | We abstract the error by generating exceptions
runErr :: ErrCV a -> a
runErr (Raise s)  = error s
runErr (Return a) = a


