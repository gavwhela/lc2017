module Handler.Profile where

import Import
import Yesod.Auth.Account (resetPasswordR)
import Yesod.Markdown

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    let bioHTML = markdownToHtml $ userBio user
    csrfToken <- fmap reqToken getRequest
    defaultLayout $ do
        setTitle . toHtml $ userUsername user <> "'s User page"
        $(widgetFile "profile")
