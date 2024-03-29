module Katip.Scribes.Raven
  ( mkRavenScribe
  ) where

import qualified Data.Aeson as Aeson
import Data.String.Conv (toS)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.HashMap.Strict as HM
import qualified Katip
import qualified Katip.Core
import qualified System.Log.Raven as Raven
import qualified System.Log.Raven.Types as Raven
import Data.Aeson.KeyMap (fromHashMapText, toHashMapText)

mkRavenScribe :: Raven.SentryService -> Katip.PermitFunc -> Katip.Verbosity -> IO Katip.Scribe
mkRavenScribe sentryService permitItem verbosity = return $
  Katip.Scribe
    { Katip.liPush = push
    , Katip.scribeFinalizer = return ()
    , Katip.scribePermitItem = permitItem
    }
  where
    push :: Katip.LogItem a => Katip.Item a -> IO ()
    push item = Raven.register sentryService (toS name) level msg updateRecord
      where
        name = sentryName $ Katip._itemNamespace item
        level = sentryLevel $ Katip._itemSeverity item
        msg = TL.unpack $ Builder.toLazyText $ Katip.Core.unLogStr $ Katip._itemMessage item
        katipAttrs =
          foldMap (\loc -> HM.singleton "loc" $ Aeson.toJSON $ Katip.Core.LocJs loc) (Katip._itemLoc item)
        updateRecord record = record
            { Raven.srEnvironment = Just $ toS $ Katip.getEnvironment $ Katip._itemEnv item
            , Raven.srTimestamp = Katip._itemTime item
            -- add katip context as raven extras
            , Raven.srExtra = convertExtra $ toHashMapText $ Katip.payloadObject verbosity (Katip._itemPayload item) <> fromHashMapText katipAttrs
            }

    convertExtra :: HM.HashMap T.Text Aeson.Value -> HM.HashMap String Aeson.Value
    convertExtra = HM.fromList . map (\(k,v) -> (T.unpack k, v)) . HM.toList

    sentryLevel :: Katip.Severity -> Raven.SentryLevel
    sentryLevel Katip.DebugS = Raven.Debug
    sentryLevel Katip.InfoS = Raven.Info
    sentryLevel Katip.NoticeS = Raven.Custom "Notice"
    sentryLevel Katip.WarningS = Raven.Warning
    sentryLevel Katip.ErrorS = Raven.Error
    sentryLevel Katip.CriticalS = Raven.Fatal
    sentryLevel Katip.AlertS = Raven.Fatal
    sentryLevel Katip.EmergencyS = Raven.Fatal

    sentryName :: Katip.Namespace -> T.Text
    sentryName (Katip.Namespace xs) = T.intercalate "." xs
