module Handler.Posts where

import Import
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))

-- Handler for an individual post
getPostR :: PostId -> Handler Html
getPostR postId = do
  muser <- maybeAuthId
  -- Get post content
  post <- runDB $ get404 postId
  -- Get the poster
  poster <- runDB $ get404 $ postAuthor post
  -- Formatted date
  let date = formatDate $ postCreated post
  -- Select comments for this post
  comments <- runDB (getComments postId)
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
  user <- requireAuthId
  time <- liftIO getCurrentTime
  (formWidget, enctype) <- generateFormPost $ newPostForm user time
  defaultLayout $ do
        setTitle "New Post"
        $(widgetFile "new-post")

postNewPostR :: Handler Html
postNewPostR = do
  user <- requireAuthId
  time <- liftIO getCurrentTime
  ((res, formWidget), enctype) <- runFormPost $ newPostForm user time
  case res of
    FormSuccess entry -> do
        runDB $ insert_ entry
        setMessage "Successfully created post"
        redirect HomeR
    _ -> defaultLayout $ do
               setTitle "New Post"
               $(widgetFile "new-post")

getComments postId = E.select $
     E.from $ \(comment `E.InnerJoin` user) -> do
        E.where_ ( comment ^. CommentPostId E.==. E.val postId )
        E.on $ comment ^. CommentUserId E.==. user ^. UserId
        return ( comment ^. CommentMessage
               , user ^. UserUsername )

-- Posting to an individual post is used to create comments
postPostR :: PostId -> Handler Html
postPostR postId = do
  user <- requireAuthId
  post <- runDB $ get404 postId
  poster <- runDB $ get404 $ postAuthor post
  -- Formatted date
  let date = formatDate $ postCreated post
  comments <- runDB (getComments postId)
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
newPostForm :: UserId -> UTCTime -> Form Post
newPostForm user currentTime = renderDivs $ Post
  <$> areq textField "Title" Nothing
  <*> pure user
  <*> pure currentTime
  <*> areq textField "Body" Nothing

-- Form to create a new comment
newCommentForm :: PostId -> UserId -> Form Comment
newCommentForm postId user = renderDivs $ Comment
  <$> areq textareaField "Message" Nothing
  <*> pure user
  <*> pure postId
