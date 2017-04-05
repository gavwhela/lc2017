module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  posts <- runDB $ selectList [] []
  defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")
