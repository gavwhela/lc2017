module Handler.Profile where

import Import
import Yesod.Auth.Account (resetPasswordR)
import Yesod.Markdown

getProfileR :: UserId -> Handler Html
getProfileR userId = do
    viewerId <- maybeAuthId
    user <- runDB $ get404 userId
    let isOwner = maybe False ((==) userId) viewerId
    let bioHTML = markdownToHtml $ userBio user
    csrfToken <- fmap reqToken getRequest
    defaultLayout $ do
        setTitle . toHtml $ userDisplayName user <> "'s User page"
        $(widgetFile "profile")
