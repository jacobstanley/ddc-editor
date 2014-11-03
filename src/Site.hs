{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module Site (app) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I

import           Application

------------------------------------------------------------------------------

routes :: [(ByteString, Handler App App ())]
routes = [ ("",          serveDirectory "static")
         ]

------------------------------------------------------------------------------

app :: SnapletInit App App
app = makeSnaplet "app" "DDC Flow UI" Nothing $ do

    h <- nestSnaplet "ignore" heist $ heistInit "templates"

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    addRoutes routes

    return (App h s)

