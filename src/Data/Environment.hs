-------------------------------------------------------------------
-- |
-- Module       : Data.Environment
-- Copyright    : (C) 2014
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Data.Environment (
    -- * Type
        Environment
    -- * Functions
    ,   environment
    ,   homeDir
    ) where

import LocalPrelude

data Environment = Environment
    {   _homeDir :: String
    }

environment :: String -> Environment
environment = Environment

homeDir :: Environment -> String
homeDir = _homeDir
