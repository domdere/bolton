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
    ,   BoltonEnvironmentError(..)
    ,   BoltonInitialiseError(..)
    ,   BoltonMakeDirError(..)
    -- * functions
    ,   initialiseBolton
    ,   runBolton
    ) where

import LocalPrelude
import Data.Environment

import Control.Exception ( SomeException )
import Control.Lens ( Iso, iso, over )
import Control.Monad ( when )
import Control.Monad.Catch ( MonadCatch(..) )
import Control.Monad.Free ( Free(..), liftF )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Either ( EitherT(EitherT), bimapEitherT, left, runEitherT )
import Data.Bool ( Bool(..) )
import Data.Either ( Either )
import Data.List ( (++) )
import System.Directory ( createDirectoryIfMissing, doesDirectoryExist )
import System.Environment ( getEnv )

data FreeBoltonF a =
        MakeBoltonDir (Either BoltonMakeDirError () -> a) String
    |   GetEnvironment (Either BoltonEnvironmentError Environment -> a)

newtype Bolton a = Bolton { _bolton :: Free FreeBoltonF a }

instance Functor FreeBoltonF where
--  fmap :: (a -> b) -> f a -> f b
    fmap f (MakeBoltonDir g s)  = MakeBoltonDir (f . g) s
    fmap f (GetEnvironment g)   = GetEnvironment (f . g)

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
    => (String -> EitherT BoltonMakeDirError m ())  -- ^ The action that attempts the create the directory
    -> EitherT BoltonEnvironmentError m Environment -- ^ The action that produces the Environment
    -> Bolton a -> m a
foldBolton mkDir getEnv'' x = let go = foldBolton mkDir getEnv'' . Bolton in case _bolton x of
    Pure y                      -> return y
    Free (MakeBoltonDir f s)    -> runEitherT (mkDir s) >>= go . f
    Free (GetEnvironment f)     -> runEitherT (getEnv'') >>= go . f

-- Error Types

data BoltonEnvironmentError = EnvVariableDoesntExist String

data BoltonMakeDirError = DirectoryAlreadyExists String

data BoltonInitialiseError =
        InitMakeDir BoltonMakeDirError
    |   InitEnv BoltonEnvironmentError

initialiseBolton :: EitherT BoltonInitialiseError Bolton ()
initialiseBolton = do
    env <- emap InitEnv getBoltonEnv
    emap InitMakeDir $ makeBoltonDir $ homeDir env ++ "/.bolton/bin"
    emap InitMakeDir $ makeBoltonDir $ homeDir env ++ "/.bolton/packages"

-- exported functions

runBolton :: Bolton a -> IO a
runBolton = foldBolton mkDirIO getEnvironmentIO

-- IO functions

mkDirIO :: String -> EitherT BoltonMakeDirError IO ()
mkDirIO d = do
    e <- lift $ doesDirectoryExist d
    when e (left $ DirectoryAlreadyExists d)
    lift $ createDirectoryIfMissing True d

getEnvironmentIO :: EitherT BoltonEnvironmentError IO Environment
getEnvironmentIO = environment <$> getEnv' "HOME"

getEnv' :: String -> EitherT BoltonEnvironmentError IO String
getEnv' s =
    let
        errorHandler :: (Monad m) => SomeException -> EitherT BoltonEnvironmentError m String
        errorHandler = const $ left $ EnvVariableDoesntExist s
    in
        (lift $ getEnv s) `catch` errorHandler

-- functions

makeBoltonDir :: String -> EitherT BoltonMakeDirError Bolton ()
makeBoltonDir = EitherT . Bolton . liftF . MakeBoltonDir id

getBoltonEnv :: EitherT BoltonEnvironmentError Bolton Environment
getBoltonEnv = EitherT $ Bolton $ liftF $ GetEnvironment id

-- helpers

emap :: (Functor m) => (e -> e') -> EitherT e m a -> EitherT e' m a
emap f = bimapEitherT f id

boltonPure :: a -> Bolton a
boltonPure = Bolton . pure

boltonBind :: Bolton a -> (a -> Bolton b) -> Bolton b
boltonBind mx f = over bolton (>>= (_bolton . f)) mx

boltonLift2 :: (a -> b -> c) -> Bolton a -> Bolton b -> Bolton c
boltonLift2 f mx my = boltonBind mx (\x -> boltonBind my (boltonPure . f x))
