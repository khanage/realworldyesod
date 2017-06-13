module ValidationErrors where

import Import

data ValidationErrors = ValidationErrors { errors :: [ValidationError]}
data ValidationError = ValidationError { name :: Text, val :: Value}

instance ToJSON ValidationErrors where
  toJSON (ValidationErrors errors) =
    object ["errors" .= object (asObject <$> errors)]
    where asObject (ValidationError name val) = name .= val

singleValidationError :: Text -> Text -> ValidationErrors
singleValidationError component reason =
  ValidationErrors [ValidationError component (toJSON reason)]

validationError :: MonadHandler m => Text -> Text -> m a
validationError field message =
  sendResponseStatus validationErrorStatus $ toJSON $ singleValidationError field message

validationErrorStatus :: Status
validationErrorStatus = Status{statusCode=422, statusMessage = "Invalid data in the request."}

