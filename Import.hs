module Import
    ( module Import
    ) where

import Foundation          as Import
import Import.NoFoundation as Import
import Tokens              as Import

emptyResponseStatus :: MonadHandler m => Status -> m a
emptyResponseStatus = flip sendResponseStatus $ toJSON ()

badAuthStatus :: Status
badAuthStatus = Status{statusCode = 401, statusMessage = "You are not authenticated"}

serverErrorStatus :: Status
serverErrorStatus = Status {statusCode = 500, statusMessage = ""}

userExistsStatus :: Status
userExistsStatus = Status{statusCode = 422, statusMessage = "User already exists."}
