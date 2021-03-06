{-# LANGUAGE DuplicateRecordFields #-}
module ApiTypes where

import Import
import Data.Aeson ((.:?))
import qualified Data.Aeson.Types as Aeson (Pair)

data UserProfile = UserProfile
  { username :: Text
  , bio :: Text
  , image :: Text
  , following :: Bool
  } deriving (Eq, Show)

instance ToJSON UserProfile where
  toJSON UserProfile{..} =
    object [ "user" .= object
             [ "username" .= username
             , "bio" .= bio
             , "image" .= image
             , "following" .= following
             ]
           ]

data IncomingUser = IncomingUser
  { username :: Text
  , email :: Text
  , password :: Text
  } deriving (Eq, Show)
instance FromJSON IncomingUser where
  parseJSON (Object v) = v .: "user" >>= \v1 ->
    IncomingUser <$> v1 .: "username"
                 <*> v1 .: "email"
                 <*> v1 .: "password"
  parseJSON _ = error "Only parses from objects"

data IncomingUserUpdate = IncomingUserUpdate
  { username :: Text
  , email :: Maybe Text
  , password :: Maybe Text
  , image :: Maybe Text
  , bio :: Maybe Text
  } deriving (Eq, Show)
instance FromJSON IncomingUserUpdate where
  parseJSON (Object wrappingObject) = wrappingObject .: "user" >>= \v ->
    IncomingUserUpdate
       <$> v .:  "username"
       <*> v .:? "email"
       <*> v .:? "password"
       <*> v .:? "image"
       <*> v .:? "bio"
  parseJSON _ = error "Only parses from objects"

data ApiUser = ApiUser
  { email :: Text
  , token :: Text
  , username :: Text
  , bio :: Maybe Text
  , image :: Maybe Text
  } deriving (Eq, Show)

instance ToJSON ApiUser where
  toJSON ApiUser {..} =
    object [ "user" .= object
             ([ "email" .= email
             , "token" .= token
             , "username" .= username
             ] ++ maybe [] (\actualBio -> ["bio" .= actualBio]) bio)
           ]

instance FromJSON ApiUser where
  parseJSON (Object wrappingObject) = wrappingObject .: "user" >>= \v ->
    ApiUser <$> v .: "username"
            <*> v .: "email"
            <*> v .: "password"
            <*> v .: "image"
            <*> v .: "bio"
  parseJSON _ = error "Only parses from objects"

data UserLogin = UserLogin { email :: Text, password :: Text}

instance FromJSON UserLogin where
  parseJSON (Object j) = do
    u <- j .: "user"
    UserLogin <$> u .: "email" <*> u .: "password"
  parseJSON _ = error "Expecting an Object"

data AuthorResult = AuthorResult
  { username :: Text
  , bio :: Maybe Text
  , image :: Maybe Text
  , following :: Bool
  } deriving (Eq, Show)

instance ToJSON AuthorResult where
  toJSON AuthorResult{..} =
    object $ [ "username" .= username
             , "bool" .= following
             ] & withMaybe "bio" bio
               & withMaybe "image" image

withMaybe :: ToJSON a => Text -> Maybe a -> [Aeson.Pair] -> [Aeson.Pair]
withMaybe named ma v =
  maybe id (\value -> (:) (named .= value)) ma $ v

data ArticleResult = ArticleResult
  { slug :: Text
  , title :: Text
  , description :: Text
  , body :: Text
  , tagList :: [TagResult]
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , favorited :: Bool
  , favoritesCount :: Int
  , author :: AuthorResult
  } deriving (Eq, Show)

instance ToJSON ArticleResult where
  toJSON ArticleResult{..} =
    object [ "article" .= object
             [ "slug" .= slug
             , "title" .= title
             , "description" .= description
             , "body" .= body
             , "tagList" .= tagList
             , "createdAt" .= createdAt
             , "updatedAt" .= updatedAt
             , "favorited" .= favorited
             , "favouritesCount" .= favoritesCount
             , "author" .= author
             ]
           ]

data TagResult = TagResult
  { tag :: Text
  } deriving (Eq, Show)

instance ToJSON TagResult where
  toJSON TagResult{..} = toJSON tag
