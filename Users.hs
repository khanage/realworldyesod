{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Users where

import           ApiTypes
import qualified Data.Map       as Map
import           Import
import qualified JwtKeys
import           Types.Password
import qualified Web.JWT        as JWT

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

apiUserFromToken
  :: Text
  -> User
  -> ApiUser
apiUserFromToken token user =
  ApiUser { email = user ^. userEmail
          , token = token
          , username = user ^. userIdent
          , bio = user ^. userBio
          , image = user ^. userImage
          }
apiUser
  :: Token -- ^ Authentication token
  -> User -- ^ User to convert
  -> ApiUser
apiUser token user = ApiUser
      { username = user^.userIdent
      , token = token
      , email = user^.userEmail
      , bio = user^.userBio
      , image = user^.userImage
      }
finalUser
  :: Monad m
  => User
  -> Token
  -> [UpdatableUserFields]
  -> m ApiUser
finalUser user token updates =
  apiUser token <$> foldlM setAllFields user updates
  where
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
  deriving (Eq, Show)

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
