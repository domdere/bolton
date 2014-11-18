-------------------------------------------------------------------
-- |
-- Module       : Control.Monad.Bolton
-- Copyright    : (C) 2014
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   :
--
-- A Free Monad for the Bolton actions.
--
-------------------------------------------------------------------
module Control.Monad.Bolton (
    -- * Type
        Bolton
    -- * Error Types
    ,   BoltonInitialiseError(..)
    ,   BoltonMakeDirError(..)
    -- * functions
    ,   initialiseBolton
    ,   runBolton
    ) where

import LocalPrelude

import Control.Lens ( Iso, (^.), iso, over )
import Control.Monad ( when )
import Control.Monad.Free ( Free(..), liftF )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Either ( EitherT(EitherT), bimapEitherT, left, runEitherT )
import Data.Bool ( Bool(..) )
import Data.Either ( Either )
import System.Directory ( createDirectoryIfMissing, doesDirectoryExist )


data FreeBoltonF a =
        MakeBoltonDir (Either BoltonMakeDirError () -> a) String

newtype Bolton a = Bolton { _bolton :: Free FreeBoltonF a }

instance Functor FreeBoltonF where
--  fmap :: (a -> b) -> f a -> f b
    fmap f (MakeBoltonDir g s) = MakeBoltonDir (f . g) s

instance Functor Bolton where
--  fmap :: (a -> b) -> f a -> f b
    fmap f = over bolton (fmap f)

instance Applicative Bolton where
--  pure :: a -> f a
    pure = boltonPure

--  (<*>) :: f (a -> b) -> f a -> f b
    (<*>) = boltonLift2 id

instance Monad Bolton where
--  return :: a -> m a
    return = boltonPure

--  (>>=) :: m a -> (a -> m b) -> m b
    (>>=) = boltonBind

-- iso

bolton :: Iso (Bolton a) (Bolton b) (Free FreeBoltonF a) (Free FreeBoltonF b)
bolton = iso _bolton Bolton

foldBolton
    :: (Monad m)
    => (String -> EitherT BoltonMakeDirError m ()) -- ^ The action that attempts the create the directory
    -> Bolton a -> m a
foldBolton mkDir x = case _bolton x of
    Pure y                      -> return y
    Free (MakeBoltonDir f s)    -> runEitherT (mkDir s) >>= foldBolton mkDir . Bolton . f

-- Error Types

data BoltonMakeDirError = DirectoryAlreadyExists String

newtype BoltonInitialiseError = BoltonInitialiseError BoltonMakeDirError

initialiseBolton :: EitherT BoltonInitialiseError Bolton ()
initialiseBolton = do
    emap BoltonInitialiseError $ makeBoltonDir "~/.bolton/bin"
    emap BoltonInitialiseError $ makeBoltonDir "~/.bolton/packages"

-- exported functions

runBolton :: Bolton a -> IO a
runBolton = foldBolton mkDirIO

-- IO functions

mkDirIO :: String -> EitherT BoltonMakeDirError IO ()
mkDirIO d = do
    e <- lift $ doesDirectoryExist d
    when e (left $ DirectoryAlreadyExists d)
    lift $ createDirectoryIfMissing True d

-- functions

makeBoltonDir :: String -> EitherT BoltonMakeDirError Bolton ()
makeBoltonDir = EitherT . Bolton . liftF . MakeBoltonDir id

-- helpers

emap :: (Functor m) => (e -> e') -> EitherT e m a -> EitherT e' m a
emap f = bimapEitherT f id

boltonPure :: a -> Bolton a
boltonPure = Bolton . pure

boltonBind :: Bolton a -> (a -> Bolton b) -> Bolton b
boltonBind mx f = over bolton (>>= (_bolton . f)) mx

boltonLift2 :: (a -> b -> c) -> Bolton a -> Bolton b -> Bolton c
boltonLift2 f mx my = boltonBind mx (\x -> boltonBind my (boltonPure . f x))
