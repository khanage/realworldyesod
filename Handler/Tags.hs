{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Tags where

import Import
import Tags (tagToTagResult)

getTagsR :: Handler Value
getTagsR = do
  logDebug $ "Loading tags"
  tags :: [Entity Tag] <- runDB $ selectList [] []
  pure $ toJSON $ tagToTagResult . entityVal <$> tags
