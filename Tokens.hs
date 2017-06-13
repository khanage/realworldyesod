module Tokens where

import Data.Time.Clock       (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Import.NoFoundation   hiding (iat)
import Web.JWT hiding (exp)
import qualified Web.JWT as JWT
import qualified Data.Aeson.Types as Aeson (parseMaybe)
import qualified JwtKeys

type Token = Text

class Monad m => MonadJwtGen m where
  jwtSecret :: m Secret

  encodeJwt :: JWTClaimsSet -> m Text
  encodeJwt c = jwtSecret <&> flip (encodeSigned HS256) c

  decodeJwt :: Text -> m (Maybe (JWT VerifiedJWT))
  decodeJwt json = jwtSecret <&> (flip decodeAndVerifySignature) json
  {-# MINIMAL jwtSecret #-}

jwtAuth :: YesodAuth m => AuthPlugin m
jwtAuth =
  AuthPlugin "jwt" dispatch login
  where
    dispatch _method _key = do
      $logDebug $ "Checking authpluin dispatch"
      notFound
    login = error "Not implemented"

jwtExpiryAfterMinutes
  :: (MonadLogger m, MonadIO m)
  => Integer
  -> m NumericDate
jwtExpiryAfterMinutes m = do
  now <- liftIO getCurrentTime
  $logDebug $ "Creating expiry for " <> tshow now
  let addedTime = fromInteger $ m * 60
      total = utcTimeToPOSIXSeconds $ addUTCTime addedTime now
  pure $ case numericDate total of
    Nothing -> error "Impossibur"
    Just t -> t

jwtStillValid
  :: (MonadLogger m, MonadIO m)
  => NumericDate
  -> m Bool
jwtStillValid date = do
  now <- liftIO $ utcTimeToPOSIXSeconds <$> getCurrentTime
  let expiry = secondsSinceEpoch date
  $logDebug $ "Comparing now: ("<> tshow now <>") to expiry ("<> tshow expiry <>")" <> tshow (now < expiry)
  pure $ now < expiry

requireValidJwt
  :: (MonadJwtGen m, MonadHandler m, MonadLogger m)
  => m AuthResult
requireValidJwt = do
  jwt <- jwtOr401
  $logDebug $ "JWT: " <> tshow jwt
  authResult <- jwtCurrent jwt
  $logDebug $ "Auth result: " <> tshow authResult
  pure authResult

jwtCurrent
  :: (MonadIO m, MonadLogger m)
  => JWT VerifiedJWT
  -> m AuthResult
jwtCurrent jwt =
  case JWT.exp (claims jwt) of
    Nothing -> pure $ Unauthorized "No expiry found"
    Just expiry -> do
      $logDebug $ "Checking if expiry " <> tshow expiry <> " is still valid"
      valid <- jwtStillValid expiry
      $logDebug $ "Expiry valid? " <> tshow valid
      pure $ if valid then Authorized else Unauthorized "Expired based on jwt"

userIdFromToken
  :: JWT VerifiedJWT
  -> Maybe UserId -- TODO: This should be a little more polymorphic
userIdFromToken jwt =
  let
    allClaims = unregisteredClaims . claims
    mValue = lookup JwtKeys.userId . allClaims
  in mValue jwt >>= Aeson.parseMaybe parseJSON

maybeJwtHeader
  :: MonadHandler m
  => MonadLogger m
  => MonadJwtGen m
  => m (Maybe (JWT VerifiedJWT))
maybeJwtHeader = do
    $logDebug "Looking up header"
    rawJwt <- lookupHeader "Authorization"
    let
        toDrop = length ("Bearer " :: Text)
        trimBearer = drop toDrop
        textJwt = trimBearer . decodeUtf8 <$> rawJwt
    $logDebug $ "Raw header: " <> tshow textJwt
    maybe (pure Nothing) decodeJwt textJwt

jwtOr401
  :: (MonadHandler m, MonadJwtGen m, MonadLogger m)
  => m (JWT VerifiedJWT)
jwtOr401 = do
  mjwt <- maybeJwtHeader
  $logDebug $ "jwtOr401: " <> tshow mjwt
  maybe notAuthenticated pure mjwt
