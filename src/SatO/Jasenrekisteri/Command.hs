module SatO.Jasenrekisteri.Command where

import Control.Lens
import Futurice.Prelude
import Prelude ()

import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

data Command
    = CmdAddTag PersonId TagName
    | CmdRemoveTag PersonId TagName
  deriving (Eq, Show)

applyCommand :: Command -> World -> World
applyCommand (CmdAddTag pid tn) w =
    w & worldMembers . ix pid . personTags . contains tn .~ True
applyCommand (CmdRemoveTag pid tn) w =
    w & worldMembers . ix pid . personTags . contains tn .~ False
