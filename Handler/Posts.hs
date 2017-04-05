module Handler.Posts where

import Import

getPostR :: PostId -> Handler Html
getPostR postId = do
  muser <- maybeAuthId
  post <- runDB $ get404 postId
  comments <- runDB $ selectList [ CommentPostId ==. postId ] []
  (formWidget, enctype) <- generateFormPost $ newCommentForm muser postId
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

postPostR :: PostId -> Handler Html
postPostR postId = do
  muser <- maybeAuthId
  post <- runDB $ get404 postId
  comments <- runDB $ selectList [ CommentPostId ==. postId ] []
  ((res, formWidget), enctype) <- runFormPost $ newCommentForm muser postId
  case res of
    FormSuccess entry -> do
        runDB $ insert entry
        setMessage "Successfully created comment"
        redirect (PostR postId)
    _ -> defaultLayout $ do
               setTitle . toHtml $ postTitle post
               $(widgetFile "post")

newPostForm :: UTCTime -> Form Post
newPostForm currentTime = renderDivs $ Post
  <$> areq textField "Title" Nothing
  <*> areq textField "Author" Nothing
  <*> pure currentTime
  <*> areq textField "Body" Nothing

newCommentForm :: Maybe UserId -> PostId -> Form Comment
newCommentForm muser postId = renderDivs $ Comment
  <$> areq textField "Message" Nothing
  <*> pure muser
  <*> pure postId