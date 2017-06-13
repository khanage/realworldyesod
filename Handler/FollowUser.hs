module Handler.FollowUser where

import Import
import Users  (userToProfile)

postUserFollowApiR
  :: Text
  -> Handler Value
postUserFollowApiR userToFollow = do
  logDebug $ "Loading auth"
  auth <- requireAuthId
  logDebug $ "Auth - " <> tshow auth

  followed <- runDB $ do
    Entity userIdToFollow followedUser <- getBy404 $ UniqueUser userToFollow
    logDebug $ "User id to follow " <> tshow userIdToFollow
    mid <- insert $ Following userIdToFollow auth
    logDebug $ "Created unique following " <> tshow mid
    pure followedUser

  pure $ toJSON $ userToProfile $ followed

deleteUserFollowApiR
  :: Text
  -> Handler Value
deleteUserFollowApiR userIdToUnfollow = do
  logDebug $ "Loading auth"
  userId <- requireAuthId
  logDebug $ "Auth - " <> tshow userId

  unfollowed <- runDB $ do
    logDebug $ "Loading user for " <> userIdToUnfollow
    Entity unfollowedId unfollowedUser <- getBy404 $ UniqueUser userIdToUnfollow
    let follow = UniqueFollowing unfollowedId userId
    logDebug $ "Loading following row"
    _ <- getBy404 follow
    logDebug $ "Deleting"
    deleteBy follow
    pure unfollowedUser

  pure $ toJSON $ userToProfile $ unfollowed
