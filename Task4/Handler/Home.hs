module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
             setTitle "Homepage"
             $(widgetFile "homepage")
