{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module Site (app) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I

import           DDC.Base.Pretty (Pretty, ppr, renderIndent)
import qualified DDC.Build.Language.Flow as Flow
import           DDC.Core.Load

import           Application

------------------------------------------------------------------------------

handleCheck :: Handler App App ()
handleCheck = method POST $ do
    (r, _) <- load <$> getRequestBody

    case r of
        Left err -> do
            writeText "Error: "
            writePretty err

        Right _  -> return ()
  where
    load = loadModuleFromString Flow.fragment "(interactive)" 1 Synth . L.unpack

writePretty :: (Pretty p, MonadSnap m) => p -> m ()
writePretty = writeText . T.pack . renderIndent . ppr

------------------------------------------------------------------------------

routes :: [(ByteString, Handler App App ())]
routes = [ ("/check", handleCheck)
         , ("",       serveDirectory "static")
         ]

------------------------------------------------------------------------------

app :: SnapletInit App App
app = makeSnaplet "app" "DDC Flow UI" Nothing $ do

    h <- nestSnaplet "ignore" heist $ heistInit "templates"

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    addRoutes routes

    return (App h s)

------------------------------------------------------------------------

program :: String
program = "module Main\n\
\export {\n\
\    main : Unit -> Unit;\n\
\}\n\
\import type {\n\
\    rT : Region;\n\
\}\n\
\import foreign c value {\n\
\        showInt   : Int# -> Ptr# rT String#;\n\
\        showNat   : Nat# -> Ptr# rT String#;\n\
\        putStrLn  : Ptr# rT String# -> Unit;\n\
\}\n\
\with letrec\n\
\\n\
\external (vs : Vector# Nat#)\n\
\ = vs\n\
\\n\
\test    (sz : Nat#)\n\
\ = do   xs  = vgenerate# sz (add# 1#)\n\
\        xs' = external xs\n\
\        y   = vreduce# add# 0# xs'\n\
\        ys  = vfilter# (lt# 5#) xs'\n\
\        z   = vreduce# add# y ys\n\
\        T2# z ys\n\
\\n\
\main (_ : Unit)\n\
\ = do   res = test 10#\n\
\        case res of\n\
\            T2# i v\n\
\             -> do putStrLn (showNat i)\n\
\                   putStrLn (showNat (vlength# v))\n\
\        ()\n\
\"
