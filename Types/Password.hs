module Types.Password
  ( Password
  , encryptPassword
  ) where

import ClassyPrelude.Yesod
import Database.Persist.Sql

newtype Password = Password { _unPassword :: Text }
  deriving (Eq, Show, IsString)

encryptPassword
  :: Monad m
  => Text
  -> m Password
encryptPassword t = pure $ Password t

instance PersistField Password where
  toPersistValue (Password t) = PersistText t
  fromPersistValue (PersistText t) = pure $ Password t
  fromPersistValue _ = Left "Failed to load password field"

instance PersistFieldSql Password where
  sqlType _ = SqlString

