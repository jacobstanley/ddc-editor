{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -w #-}

module Site (app) where

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad
import           Data.Aeson (ToJSON(..), Value(..), (.=), object)
import qualified Data.Aeson as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Char (isSpace)
import           Data.Either
import qualified Data.HashMap.Strict as H
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
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
          mb <- ppLeft (load body)
          let lets = concatMap takeExps . fst . splitXLets $ mb
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

------------------------------------------------------------------------------

handleGraph :: Handler App App ()
handleGraph = method POST $ do
    body <- getRequestBody

    let result = do
          mb <- load body
          let lets = concatMap takeExps . fst . splitXLets $ mb
              cnfs = map (second flowGraph) lets
          return cnfs

    writeLBS . A.encode . mergeChildren . toJSON $ result

------------------------------------------------------------------------------

load :: L.ByteString -> Either (Error NameF ErrorF) (Exp () NameF)
load bs = stripAnnot . moduleBody <$> loadMod bs
  where
    loadMod = fst . loadModuleFromString Flow.fragment "(interactive)" 1 Synth . L.unpack
    stripAnnot = deannotate (const Nothing) . reannotate (const ())

type NameF  = Flow.Name
type ErrorF = Flow.Error
type GraphF = Flow.Graph (Flow.CName NameF NameF) (Flow.Type NameF)

flowGraph :: ExpF -> Either Flow.ConversionError GraphF
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
-- Javascript Type System FTW

-- | Takes an array of array containing objects and merges the inner arrays
-- objects in to a single object.
--
-- Array (Array Object) ==> Array Object
--
-- For example:
--   [ [ { "foo": 1 }, { "bar": 2 } ]
--   , [ { "baz": 3 } ]
--   ]
--
-- Becomes:
--   [ { "foo": 1, "bar": 2 }
--   , { "baz": 3 }
--   ]
--
mergeChildren :: Value -> Value
mergeChildren (Array xs) = Array (V.map merge xs)
mergeChildren x          = x

-- | Merges all the objects in the array in to a single object, or is the
-- identity function if the input is not an array.
merge :: Value -> Value
merge (Array xs) = object (concatMap props (V.toList xs))
merge x          = x

-- | Gets the properties of the object, or the empty list if the value is not
-- an object.
props :: Value -> [(Text, Value)]
props (Object o) = H.toList o
props _          = []

------------------------------------------------------------------------------

instance ToJSON BindF where
  toJSON (BNone      typ) = object [ "type" .= prettyText typ ]
  toJSON (BAnon      typ) = object [ "type" .= prettyText typ ]
  toJSON (BName name typ) = object [ "type" .= prettyText typ, "name" .= prettyText name ]

instance ToJSON GraphF where
  toJSON g = object [ "nodes" .= map node ns
                    , "edges" .= map edge es ]
    where
      (ns, es) = Flow.listOfGraph g

      node (name, typ) = object [ "name" .= prettyText name
                                , "type" .= prettyText typ ]

      edge ((s, t), f) = object [ "source"  .= prettyText s
                                , "target"  .= prettyText t
                                , "canFuse" .= f ]

instance ToJSON a => ToJSON (Either (Error NameF ErrorF) a) where
  toJSON (Left  e) = object [ "error" .= prettyText e ]
  toJSON (Right x) = toJSON x

instance ToJSON a => ToJSON (Either Flow.ConversionError a) where
  toJSON (Left  e) = object [ "error" .= show e ]
  toJSON (Right x) = toJSON x

------------------------------------------------------------------------------

routes :: [(ByteString, Handler App App ())]
routes = [ ("/check", handleCheck)
         , ("/graph", handleGraph)
         , ("",       serveDirectoryWith staticConfig "static")
         ]

staticConfig :: MonadSnap m => DirectoryConfig m
staticConfig = defaultDirectoryConfig { mimeTypes = customTypes }
  where
    customTypes = H.insert ".js" "application/javascript;charset=utf-8" defaultMimeTypes

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
