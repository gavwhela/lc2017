module Handler.Posts where

import Import

getPostR :: PostId -> Handler Html
getPostR postId = do
  post <- runDB $ get404 postId
  defaultLayout $ do
        setTitle . toHtml $ postTitle post
        $(widgetFile "post")

getNewPostR :: Handler Html
getNewPostR = do
  time <- liftIO getCurrentTime
  (formWidget, enctype) <- generateFormPost $ newPostForm time
  defaultLayout $ do
        setTitle "New Post"
        $(widgetFile "new-post")

postNewPostR :: Handler Html
postNewPostR = do
  time <- liftIO getCurrentTime
  ((res, formWidget), enctype) <- runFormPost $ newPostForm time
  case res of
    FormSuccess entry -> do
        runDB $ insert_ entry
        setMessage "Successfully created post"
        redirect HomeR
    _ -> defaultLayout $ do
               setTitle "New Post"
               $(widgetFile "new-post")

newPostForm :: UTCTime -> Form Post
newPostForm currentTime = renderDivs $ Post
  <$> areq textField "Title" Nothing
  <*> areq textField "Author" Nothing
  <*> pure currentTime
  <*> areq textField "Body" Nothing
