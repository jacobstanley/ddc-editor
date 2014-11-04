{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -w #-}

module Site (app) where

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Char (isSpace)
import           Data.Either
import           Data.Text (Text)
import qualified Data.Text as T
import           Heist
import qualified Heist.Interpreted as I
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe

import           DDC.Base.Pretty (Pretty, ppr, renderIndent)
import           DDC.Core.Compounds.Simple
import           DDC.Core.Exp.Simple
import           DDC.Core.Load
import           DDC.Core.Module
import           DDC.Core.Transform.Deannotate (deannotate)
import           DDC.Core.Transform.Reannotate (reannotate)

import qualified DDC.Build.Language.Flow as Flow
import qualified DDC.Core.Flow as Flow
import           DDC.Core.Flow.Exp
import qualified DDC.Core.Flow.Transform.Rates.CnfFromExp as Flow
import qualified DDC.Core.Flow.Transform.Rates.Combinators as Flow
import qualified DDC.Core.Flow.Transform.Rates.Fail as Flow
import qualified DDC.Core.Flow.Transform.Rates.Graph as Flow
import qualified DDC.Core.Flow.Transform.Rates.SizeInference as Flow


import           Application

------------------------------------------------------------------------------

handleCheck :: Handler App App ()
handleCheck = method POST $ do
    body <- getRequestBody

    let result = do
          ModuleCore{..} <- ppLeft (load body)
          let mb   = whoNeedsAnnotationsAnyway moduleBody
              lets = concatMap takeExps . fst . splitXLets $ mb
              cnfs = map (second flowGraph) lets
          return cnfs

    case result of
        Left err  -> writeText "Error: " >> writeText err
        Right xs  -> forM_ xs $ \(b, x) -> do
            let bind = prettyText b
            writeText bind
            writeBS "\n"
            writeText (T.replicate (T.length bind) "=")

            case x of
                Left err -> do
                    writeText "\nError: "
                    writeText (T.pack (show err))

                Right g  -> do
                    let (ns, es) = Flow.listOfGraph g

                    writeText "\nNodes:"
                    forM_ ns $ \n -> do
                        let nt = prettyText n
                        writeText "\n"
                        writeText (T.dropWhile isSpace nt)

                    writeText "\n\nEdges:"
                    forM_ es $ \e -> do
                        let et = prettyText e
                        writeText "\n"
                        writeText (T.dropWhile isSpace et)

            writeBS "\n\n"
  where
    load = fst . loadModuleFromString Flow.fragment "(interactive)" 1 Synth . L.unpack

------------------------------------------------------------------------------

whoNeedsAnnotationsAnyway = deannotate (const Nothing) . reannotate (const ())

type Graph = Flow.Graph (Flow.CName Flow.Name Flow.Name)
                        (Flow.Type Flow.Name)

flowGraph :: ExpF -> Either Flow.ConversionError Graph
flowGraph exp = case Flow.cnfOfExp exp of
    Left err -> Left err
    Right x  -> Right (Flow.graphOfBinds x [])

takeExps :: LetsF -> [(BindF, ExpF)]
takeExps (LLet b e) = [(b, e)]
takeExps (LRec rbs) = rbs
takeExps _          = []

prettyText :: (Pretty p) => p -> Text
prettyText = T.pack . renderIndent . ppr

ppLeft :: Pretty e => Either e a -> Either Text a
ppLeft (Left err) = Left (T.pack (renderIndent (ppr err)))
ppLeft (Right x)  = Right x

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
