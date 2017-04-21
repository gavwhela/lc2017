module Handler.Profile where

import Import
import Yesod.Auth.Account (resetPasswordR)

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    csrfToken <- fmap reqToken getRequest
    defaultLayout $ do
        setTitle . toHtml $ userUsername user <> "'s User page"
        $(widgetFile "profile")
