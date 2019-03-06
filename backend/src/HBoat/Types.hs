module HBoat.Types where

import           Control.Comonad.Cofree
import           Data.Functor.Const

type Tagged a b = Cofree (Const b) a

data Status = Read | Unread
