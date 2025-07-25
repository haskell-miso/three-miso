
module Model where

import Miso.Lens (Lens, lens)
import Miso.Lens.TH (makeLenses)

data Model = Model
  { _mTime :: Double
  , _mRunning :: Bool
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model 0 True

