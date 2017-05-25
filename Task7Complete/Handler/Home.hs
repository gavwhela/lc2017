module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  posts <- runDB $ selectList [] [Desc PostId]
  defaultLayout $ do
        setTitle "Home"
        $(widgetFile "homepage")
