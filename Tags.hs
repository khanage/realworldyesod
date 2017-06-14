module Tags where

import Import
import ApiTypes

tagToTagResult
  :: Tag
  -> TagResult
tagToTagResult Tag{..} =
  TagResult { tag = _tagName }
