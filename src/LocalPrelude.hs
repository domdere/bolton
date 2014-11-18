-------------------------------------------------------------------
-- |
-- Module       : LocalPrelude
-- Copyright    : (C) 2014
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- The bits of the Prelude used in this project.
--
-------------------------------------------------------------------
module LocalPrelude (
    -- * Type Classes
        Functor(..)
    ,   Applicative(..)
    ,   Monad(..)
    ,   Show(..)
    ,   Eq(..)
    -- * Types
    ,   IO
    ,   String
    -- * Operators
    ,   (.)
    ,   ($)
    ,   (<$>)
    -- * Functions
    ,   id
    ,   const
    ,   flip
    ) where

import Prelude
import Control.Applicative
import Data.Functor
