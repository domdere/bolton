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
    ,   BoltonReadPackageInfoError(..)
    ,   BoltonWritePackageInfoError(..)
    ,   BoltonCdError(..)
    ,   BoltonPwdError(..)
    -- * functions
    ,   initialiseBolton
    ,   runBolton
    ) where

import LocalPrelude
import Data.Environment
import Data.PackageInfo

import Control.Exception ( SomeException )
import Control.Lens ( Iso, iso, over )
import Control.Monad ( join, when )
import Control.Monad.Catch ( MonadCatch(..) )
import Control.Monad.Free ( Free(..), liftF )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Either ( EitherT(..), bimapEitherT, hoistEither, left )
import Data.Aeson ( eitherDecode, encode )
import Data.Bool ( Bool(..) )
import Data.ByteString.Lazy ( readFile, writeFile )
import Data.Either ( Either(..) )
import Data.List ( (++) )
import System.Directory
    (   createDirectoryIfMissing
    ,   doesDirectoryExist
    ,   getCurrentDirectory
    ,   setCurrentDirectory
    )
import System.Environment ( getEnv )

data FreeBoltonF a =
        MakeBoltonDir (Either BoltonMakeDirError () -> a) String
    |   GetEnvironment (Either BoltonEnvironmentError Environment -> a)
    |   ReadPackageInfo (Either BoltonReadPackageInfoError PackageInfo -> a) String
    |   WritePackageInfo (Either BoltonWritePackageInfoError () -> a) String PackageInfo
    |   BoltonCd (Either BoltonCdError () -> a) String
    |   BoltonPwd (Either BoltonPwdError String -> a)

newtype Bolton a = Bolton { _bolton :: Free FreeBoltonF a }

instance Functor FreeBoltonF where
--  fmap :: (a -> b) -> f a -> f b
    fmap f (MakeBoltonDir g s)          = MakeBoltonDir (f . g) s
    fmap f (GetEnvironment g)           = GetEnvironment (f . g)
    fmap f (ReadPackageInfo g path)     = ReadPackageInfo (f . g) path
    fmap f (WritePackageInfo g path p)  = WritePackageInfo (f . g) path p
    fmap f (BoltonCd g path)            = BoltonCd (f . g) path
    fmap f (BoltonPwd g)                = BoltonPwd (f . g)

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
    => (String -> EitherT BoltonMakeDirError m ())                          -- ^ The action that attempts the create the directory
    -> EitherT BoltonEnvironmentError m Environment                         -- ^ The action that produces the Environment
    -> (String -> EitherT BoltonReadPackageInfoError m PackageInfo)         -- ^ The function that reads the package info
    -> (String -> PackageInfo -> EitherT BoltonWritePackageInfoError m ())  -- ^ The function that writes the package info
    -> (String -> EitherT BoltonCdError m ())                               -- ^ function that sets the current working directory
    -> EitherT BoltonPwdError m String                                      -- ^ action that gets the current working directory.
    -> Bolton a
    -> m a
foldBolton mkDir getEnv'' readInfo writeInfo cd pwd x =
    let
        go = foldBolton mkDir getEnv'' readInfo writeInfo cd pwd . Bolton
    in
        case _bolton x of
            Pure y                              -> return y
            Free (MakeBoltonDir f s)            -> runEitherT (mkDir s) >>= go . f
            Free (GetEnvironment f)             -> runEitherT (getEnv'') >>= go . f
            Free (ReadPackageInfo f path)       -> runEitherT (readInfo path) >>= go . f
            Free (WritePackageInfo f path p)    -> runEitherT (writeInfo path p) >>= go . f
            Free (BoltonCd f path)              -> runEitherT (cd path) >>= go . f
            Free (BoltonPwd f)                  -> runEitherT pwd >>= go . f

-- Error Types

data BoltonEnvironmentError = EnvVariableDoesntExist String

instance Show BoltonEnvironmentError where
--  show :: a -> String
    show (EnvVariableDoesntExist envvar) = "Environment Variable not set: '" ++ envvar ++ "'"

data BoltonMakeDirError = DirectoryAlreadyExists String

instance Show BoltonMakeDirError where
--  show :: a -> String
    show (DirectoryAlreadyExists p) = "Directory Already Exists: '" ++ p ++ "'"

data BoltonInitialiseError =
        InitMakeDir BoltonMakeDirError
    |   InitEnv BoltonEnvironmentError

instance Show BoltonInitialiseError where
--  show :: a -> String
    show (InitMakeDir x)    = show x
    show (InitEnv x)        = show x

data BoltonReadPackageInfoError =
        PackageInfoFromJSON String String
    |   ReadIOError String String

instance Show BoltonReadPackageInfoError where
--  show :: a -> String
    show (PackageInfoFromJSON path pError) = join
        [   "Parse error reading package metadata at '"
        ,   path
        ,   "': "
        ,   pError
        ]
    show (ReadIOError path ioError) = join
        [   "Encountered Read Error for '"
        ,   path
        ,   "': "
        ,   ioError
        ]

data BoltonWritePackageInfoError =
    WriteIOError String String

instance Show BoltonWritePackageInfoError where
--  show :: a -> String
    show (WriteIOError path ioError) = join
        [   "Encountered Write Error for '"
        ,   path
        ,   "'"
        ,   "': "
        ,   ioError
        ]

data BoltonCdError = BoltonCdIOError String String

instance Show BoltonCdError where
--  show :: a -> String
    show (BoltonCdIOError path err) = join
        [   "[IO] Could not set current working directory to '"
        ,   path
        ,   "', Error: '"
        ,   err
        ,   "'"
        ]

data BoltonPwdError = BoltonPwdIOError String

instance Show BoltonPwdError where
--  show :: a -> String
    show (BoltonPwdIOError err) = join
        [   "[IO] Could not get current working directory, Error: '"
        ,   err
        ,   "'"
        ]

-- exported functions

initialiseBolton :: EitherT BoltonInitialiseError Bolton ()
initialiseBolton = do
    env <- emap InitEnv getBoltonEnv
    emap InitMakeDir $ makeBoltonDir $ binDir env
    emap InitMakeDir $ makeBoltonDir $ packagesDir env

runBolton :: Bolton a -> IO a
runBolton = foldBolton mkDirIO getEnvironmentIO readPackageInfoIO writePackageInfoIO boltonCdIO boltonPwdIO

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

readPackageInfoIOUnsafe :: String -> EitherT BoltonReadPackageInfoError IO PackageInfo
readPackageInfoIOUnsafe metaPath = (lift $ readFile metaPath) >>= emap (PackageInfoFromJSON metaPath) . hoistEither . eitherDecode

readPackageInfoIO :: String -> EitherT BoltonReadPackageInfoError IO PackageInfo
readPackageInfoIO metaPath = (readPackageInfoIOUnsafe metaPath) `catch` (ioErrorCatcher $ ReadIOError metaPath)

writePackageInfoIO :: String -> PackageInfo -> EitherT BoltonWritePackageInfoError IO ()
writePackageInfoIO metaPath pInfo = (lift $ writeFile metaPath (encode pInfo)) `catch` (ioErrorCatcher $ WriteIOError metaPath)

boltonCdIO :: String -> EitherT BoltonCdError IO ()
boltonCdIO path = (lift $ setCurrentDirectory path) `catch` (ioErrorCatcher $ BoltonCdIOError path)

boltonPwdIO :: EitherT BoltonPwdError IO String
boltonPwdIO = (lift getCurrentDirectory) `catch` (ioErrorCatcher BoltonPwdIOError)

-- Bolton functions

makeBoltonDir :: String -> EitherT BoltonMakeDirError Bolton ()
makeBoltonDir = EitherT . Bolton . liftF . MakeBoltonDir id

getBoltonEnv :: EitherT BoltonEnvironmentError Bolton Environment
getBoltonEnv = EitherT $ Bolton $ liftF $ GetEnvironment id

readPackageInfo :: String -> EitherT BoltonReadPackageInfoError Bolton PackageInfo
readPackageInfo = EitherT . Bolton . liftF . ReadPackageInfo id

writePackageInfo :: String -> PackageInfo -> EitherT BoltonWritePackageInfoError Bolton ()
writePackageInfo = ((EitherT . Bolton . liftF) .) . WritePackageInfo id

boltonCd :: String -> EitherT BoltonCdError Bolton ()
boltonCd = EitherT . Bolton . liftF . BoltonCd id

boltonPwd :: EitherT BoltonPwdError Bolton String
boltonPwd = EitherT $ Bolton $ liftF $ BoltonPwd id

-- important values

boltonStore :: Environment -> String
boltonStore env = homeDir env ++ "/.bolton"

binDir :: Environment -> String
binDir env = boltonStore env ++ "/bin"

packagesDir :: Environment -> String
packagesDir env = boltonStore env ++ "/packages"

packageDir :: Environment -> String -> String
packageDir env package = packagesDir env ++ "/" ++ package

packageMetadata :: Environment -> String -> String
packageMetadata env package = packageDir env package ++ "/metadata.json"

-- helpers

ioErrorCatcher
    :: (Monad m)
    => (String -> e)
    -> SomeException
    -> EitherT e m a
ioErrorCatcher f = left . f . show

emap :: (Functor m) => (e -> e') -> EitherT e m a -> EitherT e' m a
emap f = bimapEitherT f id

boltonPure :: a -> Bolton a
boltonPure = Bolton . pure

boltonBind :: Bolton a -> (a -> Bolton b) -> Bolton b
boltonBind mx f = over bolton (>>= (_bolton . f)) mx

boltonLift2 :: (a -> b -> c) -> Bolton a -> Bolton b -> Bolton c
boltonLift2 f mx my = boltonBind mx (\x -> boltonBind my (boltonPure . f x))
