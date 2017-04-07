module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  -- Fetch list of posts for homepage
  posts <- runDB $ selectList [] []
  defaultLayout $ do
        setTitle "Blog!"
        $(widgetFile "homepage")
