module Handler.Secret where

import Import

getSecretR :: Handler Html
getSecretR = do
  user <- requireAuthId
  defaultLayout $ do
        setTitle "Secret"
        $(widgetFile "secret")

getSuperSecretR :: Handler Html
getSuperSecretR = do
  defaultLayout $ do
        setTitle "Super Secret"
        $(widgetFile "super-secret")
