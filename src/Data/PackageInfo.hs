{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Data.PackageInfo
-- Copyright    : (C) 2014
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Data.PackageInfo (
    -- * Types
        HackageLocation(..)
    ,   PackageSource(..)
    ,   PackageInfo(..)
    ) where

import LocalPrelude

import Control.Monad ( join )
import Data.Aeson ( FromJSON(..), ToJSON(..), Value(..), (.:), (.=), object )
import Data.Aeson.Types ( typeMismatch )
import Data.Either ( Either(..) )
import Data.List ( (++), intercalate )
import Data.Text ( Text )

import GHC.Generics ( Generic )

data HackageLocation = HackageLocation
    {   version :: String
    }   deriving (Eq, Generic)

instance Show HackageLocation where
--  show :: a -> String
    show x = "Hackage (version: " ++ version x ++ ")"

instance FromJSON HackageLocation
instance ToJSON HackageLocation

data PackageSource =
    Hackage HackageLocation

instance FromJSON PackageSource where
--  parseJSON :: Value -> Parser a
    parseJSON (Object v)    = do
        ty <- tyFromString <$> v .: "source"
        case ty of
            Right THackage  -> Hackage <$> v .: "version_info"
            Left s          -> fail s
    parseJSON v             = typeMismatch "PackageSource" v

instance ToJSON PackageSource where
--  toJSON :: a -> Value
    toJSON (Hackage v) = object
        [   "source"        .= ("hackage" :: Text)
        ,   "version_info"  .= v
        ]

instance Show PackageSource where
--  show :: a -> String
    show (Hackage x) = show x

data PackageInfo = PackageInfo
    {   name        :: String
    ,   location    :: PackageSource
    ,   binaries    :: [String]
    }   deriving (Generic)

instance FromJSON PackageInfo
instance ToJSON PackageInfo

instance Show PackageInfo where
--  show :: a -> String
    show x = join
        [   name x
        ,   " (Source: "
        ,   show (location x)
        ,   "): "
        ,   intercalate ", " (binaries x)
        ]

-- helpers

data PackageType = THackage

tyFromString :: String -> Either String PackageType
tyFromString "hackage"  = Right THackage
tyFromString s          = Left $ "Invalid Package source type: '" ++ s ++ "'"
