module Handler.Post where

import Import

getPostR :: PostId -> Handler Html
getPostR postId = do
  post <- runDB $ get404 postId
  defaultLayout $ do
    setTitle $ toHtml $ postTitle post
    $(widgetFile "post")

getNewPostR :: Handler Html
getNewPostR = postNewPostR

postNewPostR :: Handler Html
postNewPostR = do
  user <- requireAuthId
  ((res, formWidget), enctype) <- runFormPost $ postForm user
  case res of
    FormSuccess post -> do
           postId <- runDB $ insert post
           redirect (PostR postId)
    _ -> defaultLayout $ do
              setTitle "New Post"
              $(widgetFile "new-post")

postForm :: Text -> Form Post
postForm user = renderDivs $ Post
    <$> areq textField "Title" Nothing
    <*> pure user
    <*> areq textareaField "Contents" Nothing
