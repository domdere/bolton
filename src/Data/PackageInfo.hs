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
    ) where

import LocalPrelude

import Data.Aeson ( FromJSON(..), ToJSON(..), Value(..), (.:), (.=), object )
import Data.Aeson.Types ( typeMismatch )
import Data.Either ( Either(..) )
import Data.Maybe ( Maybe, fromMaybe )
import Data.List ( (++) )
import Data.Text ( Text )

import GHC.Generics ( Generic )

data HackageLocation = HackageLocation
    {   version :: Maybe String
    }   deriving (Eq, Generic)

instance Show HackageLocation where
--  show :: a -> String
    show x = "Hackage (version: " ++ fromMaybe "latest" (version x) ++ ")"

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

-- helpers

data PackageType = THackage

tyFromString :: String -> Either String PackageType
tyFromString "hackage"  = Right THackage
tyFromString s          = Left $ "Invalid Package source type: '" ++ s ++ "'"
