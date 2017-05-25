module Handler.Message where

import Import

getMessageR :: Handler Html
getMessageR = do
  setMessage "This is my message!"
  redirect HomeR
