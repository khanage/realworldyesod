{-# LANGUAGE ScopedTypeVariables #-}
module Handler.UserApi where

import ApiTypes
import Import
import Types.Password
import Users
import ValidationErrors

-- | Load the profile for a given user.
getUserProfileApiR
  :: Text -- ^ The name of the profile to lookup, i.e. the username.
  -> Handler Value
getUserProfileApiR name = do
  Entity _ user <- runDB $ getBy404 $ UniqueUser name
  let profile = userToProfile user
  logDebug $ "Got user: " <> tshow profile
  pure $ toJSON profile

-- | Return the profile of the currently logged in user.
getUserApiR :: Handler Value
getUserApiR = do
  logDebug "Loading auth"
  authId <- requireAuthId

  logDebug $ "Auth loaded"
  runDB $ get authId >>= maybe (notFoundUser authId) foundUser

  where
    foundUser = pure . toJSON . userToProfile
    notFoundUser authId = do
      logError $ "Could not find user that was logged in as " <> tshow authId
      emptyResponseStatus serverErrorStatus

-- | Create a new user.
postUserApiR :: Handler Value
postUserApiR = do
  IncomingUser{..} <- requireJsonBody
  pass <- encryptPassword password
  mres <- runDB $ do
    mkey <- insertUnique $ buildUser username pass email
    maybe (pure Nothing) getEntity mkey

  maybe badRequest createUser mres

  where
    badRequest = validationError "user" "User already exists."

    createUser userEntity = do
      token <- genJwtFor userEntity
      pure $ toJSON $ apiUserFromToken token $ entityVal userEntity

-- | Update a given user.
putUserApiR :: Handler Value
putUserApiR = do
  logDebug $ "Loading `IncomingUserUpdate`"
  incomingUser :: IncomingUserUpdate <- requireJsonBody

  let username = uniqueUsername incomingUser
      updates = updatesToMake incomingUser
  logDebug $ "Updates: " <> tshow updates

  user <- loadAndUpdate username updates
  logDebug $ "Loaded user and updates"

  token <- genJwtFor user
  logDebug $ "Token generated"

  pure $ toJSON $ apiUser token $ entityVal user

  where
    uniqueUsername = UniqueUser . \IncomingUserUpdate{..} -> username

    loadAndUpdate uniqueUser allUpdates = runDB $ do
      Entity userId _ <- getBy404 uniqueUser
      updates <- dbUpdates allUpdates
      user <- updateGet userId updates
      pure $ Entity userId user

-- | Login as a user.
postLoginApiR :: Handler Value
postLoginApiR = do
  UserLogin{..} <- requireJsonBody
  userEntity <- runDB $ getBy404 $ UniqueEmail email
  pass <- encryptPassword password

  if pass `matchesPassFor` userEntity
    then loggedIn userEntity
    else noLoginForYou

  where
    pass `matchesPassFor` (Entity _ user) =
      pass == user ^. userPassword

    noLoginForYou =
      validationError "password" "Invalid password."

    loggedIn userEntity = do
      jwt <- genJwtFor userEntity
      pure $ toJSON $ apiUser jwt (entityVal userEntity)
