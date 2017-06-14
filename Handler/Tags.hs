{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Tags where

import Import
import ApiTypes (TagResult(..))

getTagsR :: Handler Value
getTagsR = do
  logDebug $ "Loading tags"
  tags :: [Entity Tag] <- runDB $ selectList [] []
  pure $ toJSON $ tagToResult . entityVal <$> tags

tagToResult
  :: Tag
  -> TagResult
tagToResult Tag{..} =
  TagResult { tag = _tagName }
