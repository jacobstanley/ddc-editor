module Application where

import Control.Applicative ((<$>))
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Session

------------------------------------------------------------------------------

data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    }

-- Can't use TemplateHaskell with limp-cbc :(
-- $(makeLenses ''App)

heist :: Lens' App (Snaplet (Heist App))
heist inj (App h s) = (\h' -> App h' s) <$> inj h
{-# INLINE heist #-}

sess :: Lens' App (Snaplet SessionManager)
sess inj (App h s) = (\s' -> App h s') <$> inj s
{-# INLINE sess #-}

instance HasHeist App where
    heistLens = subSnaplet heist

type AppHandler = Handler App App
