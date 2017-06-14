module Handler.Articles where

import Import
import Articles

getSingleArticleR
  :: Text
  -> Handler Value
getSingleArticleR slug = do
  logDebug $ "Loading article for slug: " <> slug
  article <- runDB $ getBy404 $ UniqueSlug slug
  pure $ toJSON $ articleToArticleResult $ entityVal $ article
