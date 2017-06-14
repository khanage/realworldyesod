module Articles where

import           ApiTypes
import           Database.Esqueleto (from, on, select, val, where_, (&&.),
                                     (==.))
import qualified Database.Esqueleto as E
import           Import             hiding (from, on, (==.))
import           Tags

(^^.) :: (PersistField typ, E.Esqueleto query expr backend, PersistEntity val)
      => expr (Entity val)
      -> EntityField val typ
      -> expr (E.Value typ)
body ^^. field = body E.^. field

-- | Maps article pieces to the result expected by
-- the api.
articleToArticleResult
  :: [Tag]   -- ^ The list of tags
  -> User    -- ^ The author (just a user at this point)
  -> Bool    -- ^ Whether or not the current user has favorited the article
  -> Bool    -- ^ Following author
  -> Int     -- ^ Number of favorites for the article
  -> Article -- ^ The article model
  -> ArticleResult
articleToArticleResult tags user currentUserFavorited followingAuthor numberOfFavorites Article{..} =
  ArticleResult
  { slug = _articleSlug
  , title = _articleTitle
  , description = _articleDescription
  , body = _articleBody
  , createdAt = _articleCreatedAt
  , updatedAt = _articleUpdatedAt
  , tagList = tagToTagResult <$> tags
  , favorited = currentUserFavorited
  , favoritesCount = numberOfFavorites
  , author = userToAuthor user followingAuthor
  }

userToAuthor
  :: User
  -> Bool
  -> AuthorResult
userToAuthor User{..} following =
  AuthorResult
    { username = _userIdent
    , bio = _userBio
    , image = _userImage
    , following = following
    }

-- | Load an article result for a slug.
articleResultFromSlug
  :: Text
  -> Handler (Maybe ArticleResult)
articleResultFromSlug slug = do
  mauthId <- maybeAuthId

  runDB $ do
    articleUserResult <- listToMaybe <$> articleBySlug slug
    sequence $ loadAllData mauthId <$> articleUserResult

  where

    -- loadAllData
    --   :: ( SqlBackend ~ BaseBackend backend
    --      , IsPersistBackend backend
    --      , PersistQueryRead backend
    --      , PersistUniqueRead backend
    --      , MonadIO m)
    --   => Maybe (Key User)
    --   -> (Entity Article, Entity User)
    --   -> ReaderT backend m ArticleResult
    loadAllData mauthId (article, author) = do
        let articleId = entityKey article
            authorId = entityKey author
            authorVal = entityVal author
            articleVal = entityVal article

        tags <- fmap entityVal <$> tagEntitiesForArticle articleId

        numFavs <- maybe 0 E.unValue . headMay <$> favsForArticle articleId

        favorited <- not.null <$> userFavorited articleId authorId

        userFollowsAuthor <- maybe (pure False) (followsAuthor authorId) mauthId

        pure $ articleToArticleResult tags authorVal favorited userFollowsAuthor numFavs articleVal

    followsAuthor authorId userId =
      trueIfResult $
      select $ from $ \(follower `E.InnerJoin` f `E.InnerJoin` followed) -> do
      on (follower ^^. UserId ==. f ^^. FollowingFollower)
      on (followed ^^. UserId ==. f ^^. FollowingFollowed)
      where_ (followed ^^. UserId ==. val authorId
          &&. follower ^^. UserId ==. val userId)
      pure (follower, followed)

    trueIfResult :: Functor f => f [b] -> f Bool
    trueIfResult = fmap (not . null)

    articleBySlug suppliedSlug =
      select $ from $ \(article `E.InnerJoin` user) -> do
      on (article ^^. ArticleAuthorId ==. user ^^. UserId)
      where_ (article ^^. ArticleSlug ==. val suppliedSlug)
      pure (article, user)

    tagEntitiesForArticle articleId =
      select $
      from $ \(articleTag `E.InnerJoin` tag) -> do
      on (articleTag ^^. Article_TagTag ==. tag ^^. TagId)
      where_ (articleTag ^^. Article_TagArticle ==. val articleId)
      pure tag

    favsForArticle articleId =
      select $ from $ \fav -> do
      where_ (fav ^^. UserFavoriteArticle ==. val articleId)
      pure (E.countRows :: E.SqlExpr (E.Value Int))

    userFavorited articleId userId =
      select $ from $ \fav -> do
      where_ (fav ^^. UserFavoriteArticle ==. val articleId
          &&. fav ^^. UserFavoriteUser ==. val userId)
