{-# LANGUAGE CPP #-}
module Import.NoFoundation
    ( module Import
    , lookupHeader404
    ) where

import ClassyPrelude.Yesod            as Import hiding (logDebug, logError,
                                                 logInfo, logWarn)
import Control.Lens                   as Import hiding (Index, cons, index,
                                                 snoc, uncons, unsnoc, (.=),
                                                 (<.), (<.>), (<|), (|>))
import Control.Monad.Logger.CallStack as Import (logDebug, logError, logInfo,
                                                 logWarn)
import Control.Monad.Trans.Maybe      as Import
import Data.CaseInsensitive
import Model                          as Import
import Settings                       as Import
import Settings.StaticFiles           as Import
import Yesod.Auth                     as Import
import Yesod.Core.Types               as Import (loggerSet)
import Yesod.Default.Config2          as Import

lookupHeader404 :: MonadHandler m => CI ByteString -> m ByteString
lookupHeader404 name = do
  mheader <- lookupHeader name
  case mheader of
    Nothing -> notFound
    Just value -> pure value
