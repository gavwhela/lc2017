module Handler.Posts where

import Import

-- Handler for an individual post
getPostR :: PostId -> Handler Html
getPostR postId = do
  muser <- maybeAuthId
  -- Get post content
  post <- runDB $ get404 postId
  -- Formatted date
  let date = formatDate $ postCreated post
  -- Select comments for this post
  comments <- runDB $ selectList [ CommentPostId ==. postId ] []
  -- Generate a form for creating new comments
  mform <- traverse (generateFormPost . newCommentForm postId) muser
  defaultLayout $ do
        setTitle . toHtml $ postTitle post
        $(widgetFile "post")

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale dateFormat
    where dateFormat = "%a, %B %e, %0Y"

-- Handler for page to create new posts
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

-- Posting to an individual post is used to create comments
postPostR :: PostId -> Handler Html
postPostR postId = do
  user <- requireAuthId
  post <- runDB $ get404 postId
  -- Formatted date
  let date = formatDate $ postCreated post
  comments <- runDB $ selectList [ CommentPostId ==. postId ] []
  ((res, formWidget), enctype) <- runFormPost $ newCommentForm postId user
  case res of
    FormSuccess entry -> do
        runDB $ insert_ entry
        setMessage "Successfully created comment"
        redirect (PostR postId)
    _ -> defaultLayout $ do
               setTitle . toHtml $ postTitle post
               let mform = Just (formWidget, enctype)
               $(widgetFile "post")

-- Form to create a new post
newPostForm :: UTCTime -> Form Post
newPostForm currentTime = renderDivs $ Post
  <$> areq textField "Title" Nothing
  <*> areq textField "Author" Nothing
  <*> pure currentTime
  <*> areq textField "Body" Nothing

-- Form to create a new comment
newCommentForm :: PostId -> UserId -> Form Comment
newCommentForm postId user = renderDivs $ Comment
  <$> areq textareaField "Message" Nothing
  <*> pure user
  <*> pure postId
