module Katip.Scribes.Raven
  ( mkRavenScribe
  ) where

import Data.String.Conv (toS)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as Builder
import qualified Katip
import qualified System.Log.Raven as Raven
import qualified System.Log.Raven.Types as Raven


mkRavenScribe :: Raven.SentryService -> Katip.PermitFunc -> Katip.Verbosity -> IO Katip.Scribe
mkRavenScribe sentryService permitItem _ = return $ 
  Katip.Scribe
    { Katip.liPush = push
    , Katip.scribeFinalizer = return ()
    , Katip.scribePermitItem = permitItem
    }
  where
    push :: Katip.Item a -> IO ()
    push item = Raven.register sentryService (toS name) level (toS msg) updateRecord
      where
        name = sentryName $ Katip._itemNamespace item
        level = sentryLevel $ Katip._itemSeverity item
        msg = Builder.toLazyText $ Katip.unLogStr $ Katip._itemMessage item
        updateRecord record = record 
            { Raven.srEnvironment = Just $ toS $ Katip.getEnvironment $ Katip._itemEnv item
            , Raven.srTimestamp = show $ Katip._itemTime item
            -- TODO: unpack request: 
            --Katip.payloadObject verbosity $ Katip._itemPayload
            }

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
