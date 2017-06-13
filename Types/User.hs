{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Types.User where

import           ApiTypes
import qualified Data.Map       as Map
import           Import
import qualified JwtKeys
import           Types.Password
import qualified Web.JWT        as JWT

data UserLogin = UserLogin { email :: Text, password :: Text}

instance FromJSON UserLogin where
  parseJSON (Object j) = do
    u <- j .: "user"
    UserLogin <$> u .: "email" <*> u .: "password"
  parseJSON _ = error "Expecting an Object"

buildUser
  :: Text
  -> Password
  -> Text
  -> User
buildUser username pass email = User
      { _userIdent = username
      , _userPassword = pass
      , _userEmail = email
      , _userBio = Nothing
      , _userImage = Nothing
      }

finalUser
  :: Monad m
  => User
  -> Token
  -> [UpdatableUserFields]
  -> m ApiUser
finalUser user token updates =
  apiUser <$> foldlM setAllFields user updates
  where
    apiUser u = ApiUser
      { username = u^.userIdent
      , token = token
      , email = u^.userEmail
      , bio = u^.userBio
      , image = u^.userImage
      }
    setAllFields u utm = case utm of
      UpdateUsername n -> pure $ u & userIdent .~ n
      UpdateEmail e -> pure $ u & userEmail .~ e
      UpdatePassword p -> do
        password <- encryptPassword p
        pure $ u & userPassword .~ password
      UpdateImage i -> pure $ u & userImage ?~ i
      UpdateBio b -> pure $ u & userBio ?~ b

userToProfile :: User -> UserProfile
userToProfile user =
  UserProfile { username = user^.userIdent
              , bio = user^.userBio . to (fromMaybe "")
              , image = user^.userImage . to (fromMaybe "")
              , following = False
              }

data UpdatableUserFields
  = UpdateEmail Text
  | UpdateUsername Text
  | UpdatePassword Text
  | UpdateImage Text
  | UpdateBio Text

updatesToMake
  :: IncomingUserUpdate
  -> [UpdatableUserFields]
updatesToMake IncomingUserUpdate{..} =
  catMaybes $
      [ email    <&> UpdateEmail
      , password <&> UpdatePassword
      , image    <&> UpdateImage
      , bio      <&> UpdateBio
      ]

dbUpdates
  :: Monad m
  => [UpdatableUserFields]
  -> m [Update User]
dbUpdates =
  traverse $ \case
     UpdateUsername u -> pure $ UserIdent =. u
     UpdateEmail e    -> pure $ UserEmail =. e
     UpdatePassword p -> do
       pass <- encryptPassword p
       pure $ UserPassword =. pass
     UpdateImage i    -> pure $ UserImage =. Just i
     UpdateBio b      -> pure $ UserBio =. Just b

genJwtFor
  :: (MonadIO m, MonadJwtGen m, MonadLogger m, MonadReader App m)
  => Entity User
  -> m Text
genJwtFor (Entity userId user) = do
  expiryMins <- asks (appJwtExpiryMinutes . appSettings)
  expiry <- jwtExpiryAfterMinutes expiryMins
  encodeJwt $ def { JWT.iss = JWT.stringOrURI "realworldyesod"
                  , JWT.exp = Just expiry
                  , JWT.unregisteredClaims = Map.fromList
                    [ (JwtKeys.isUser, Bool True)
                    , (JwtKeys.primaryEmail, String (user^.userEmail))
                    , (JwtKeys.userId, toJSON userId)
                    ]
                  }
