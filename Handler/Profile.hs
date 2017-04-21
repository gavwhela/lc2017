module Handler.Profile where

import Import
import Yesod.Auth.Account (resetPasswordR)
import Yesod.Markdown

getProfileR :: UserId -> Handler Html
getProfileR userId = do
    viewerId <- requireAuthId
    user <- runDB $ get404 userId
    let isOwner = viewerId == userId
    let bioHTML = markdownToHtml $ userBio user
    csrfToken <- fmap reqToken getRequest
    defaultLayout $ do
        setTitle . toHtml $ userDisplayName user <> "'s User page"
        $(widgetFile "profile")
