module Handler.Posts where

import Import
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import Yesod.Markdown
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.Text.Lazy.Encoding as LE (decodeUtf8)
import Text.Julius (rawJS)

-- Handler for an individual post
getPostR :: PostId -> Handler Html
getPostR postId = do
  muser <- maybeAuthId
  -- Get post content or 404 if doesn't exist
  post <- runDB $ get404 postId
  -- Get the poster
  poster <- runDB $ get404 $ postAuthor post
  -- Formatted date
  let date = formatDate $ postCreated post
  -- Select comments for this post
  comments <- runDB (getComments postId)
  -- Generate HTML for markdown post
  let postHTML = markdownToHtml $ postBody post
  let isOwner = maybe False ((==) $ postAuthor post) muser
  -- Generate a form for creating new comments
  mform <- traverse (generateFormPost . newCommentForm postId) muser
  defaultLayout $ do
        setTitle . toHtml $ postTitle post
        $(widgetFile "post")

-- Posting to an individual post is used to create comments
postPostR :: PostId -> Handler Html
postPostR postId = do
  -- User ID required to post a comment.
  user <- requireAuthId
  let muser = Just user
  -- Get post content or 404 if doesn't exist
  post <- runDB $ get404 postId
  -- Get the poster
  poster <- runDB $ get404 $ postAuthor post
  -- Formatted date
  let date = formatDate $ postCreated post
  -- Select comments for this post
  comments <- runDB (getComments postId)
  -- Generate HTML for markdown post
  let postHTML = markdownToHtml $ postBody post
  let isOwner = user == postAuthor post
  -- Generate a form for creating new comments
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

-- Delete a post
deletePostR :: PostId -> Handler Html
deletePostR postId = do
  -- User ID required to delete post
  userId <- requireAuthId
  -- Get post content or 404 if doesn't exist
  post <- runDB $ get404 postId
  -- Return 403 if user does not own post
  unless (userId == postAuthor post) (permissionDenied "You do not own that post")
  -- Actually delete post and comments on post
  runDB $ do
    deleteWhere [CommentPostId ==. postId]
    delete postId
  -- Set success message in session
  setMessage "Deleted Post"
  -- Redirect to post list
  redirect HomeR

-- Esqueleto query to fetch comments for a given post ID
getComments :: Key Post -> ReaderT SqlBackend Handler [( E.Value Textarea
                                                       , E.Value CommentId
                                                       , E.Value Text
                                                       , E.Value UserId)]
getComments postId = E.select $
     -- Join comments and users to get display names
     E.from $ \(comment `E.InnerJoin` user) -> do
        -- Only posts for this post ID
        E.where_ ( comment ^. CommentPostId E.==. E.val postId )
        -- Order newest first
        E.orderBy [E.desc $ comment ^. CommentId]
        -- Join users to comments when the ids match
        E.on $ comment ^. CommentUserId E.==. user ^. UserId
        -- Only project out the needed fields, display name and comment message
        return ( comment ^. CommentMessage
               , comment ^. CommentId
               , user ^. UserDisplayName
               , user ^. UserId)

deleteCommentR :: CommentId -> Handler ()
deleteCommentR commentId = do
  -- User ID required to delete comment
  userId <- requireAuthId
  -- Get comment content or 404 if doesn't exist
  comment <- runDB $ get404 commentId
  -- Return 403 if user does not own comment
  unless (userId == commentUserId comment) (permissionDenied "You do not own that comment")
  -- Actually delete comment
  runDB $ delete commentId

-- Format a UTCTime into a human readable date
formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale dateFormat
    where dateFormat = "%a, %B %e, %0Y"

parsePreview :: Value -> Parser Markdown
parsePreview = withObject "preview" (\obj -> do
                                       markdown <- obj .: "markdown"
                                       return $ Markdown markdown)

-- Handler takes JSON markdown and return JSON of the markdown rendered into HTML
postPreviewR :: Handler Value
postPreviewR = do
  mMarkdown <- do rval <- parseCheckJsonBody
                  case rval of
                    Error _ -> return Nothing
                    Success val -> return $ parseMaybe parsePreview val
  let failed = return $ object [ "status" .= ("fail" :: Text) ]
  case mMarkdown of
    Nothing -> failed
    Just markdown ->
        do let mpreviewHtml = markdownToHtml markdown
           case mpreviewHtml of
             Left _ -> failed
             Right previewHtml ->
               return $ object [ "status" .= ("success" :: Text)
                               , "html" .= (LE.decodeUtf8 $ renderHtml previewHtml)]

-- Handler for page to create new posts
getNewPostR :: Handler Html
getNewPostR = do
  -- User ID required to create posts
  user <- requireAuthId
  time <- liftIO getCurrentTime
  bodyId <- newIdent
  formId <- newIdent
  let submitRoute = NewPostR
      header = "New Post" :: Text
  (formWidget, enctype) <- generateFormPost $ newPostForm user time Nothing Nothing bodyId
  defaultLayout $ do
        setTitle "New Post"
        $(widgetFile "new-post")

postNewPostR :: Handler Html
postNewPostR = do
  -- User ID required to create posts
  user <- requireAuthId
  time <- liftIO getCurrentTime
  bodyId <- newIdent
  formId <- newIdent
  let submitRoute = NewPostR
      header = "New Post" :: Text
  ((res, formWidget), enctype) <- runFormPost $ newPostForm user time Nothing Nothing bodyId
  case res of
    FormSuccess entry -> do
        runDB $ insert_ entry
        setMessage "Successfully created post"
        redirect HomeR
    _ -> defaultLayout $ do
               setTitle "New Post"
               $(widgetFile "new-post")

getEditPostR :: PostId -> Handler Html
getEditPostR postId = do
  user <- requireAuthId
  post <- runDB $ get404 postId
  bodyId <- newIdent
  formId <- newIdent
  let submitRoute = EditPostR postId
      header = "Edit Post" :: Text
      created = postCreated post
      lastTitle = Just $ postTitle post
      lastBody = Just $ postBody post
  (formWidget, enctype) <- generateFormPost $ newPostForm user created lastTitle lastBody bodyId
  defaultLayout $ do
    setTitle "Edit Post"
    $(widgetFile "new-post")

postEditPostR :: PostId -> Handler Html
postEditPostR postId = do
  user <- requireAuthId
  post <- runDB $ get404 postId
  bodyId <- newIdent
  formId <- newIdent
  let submitRoute = EditPostR postId
      header = "Edit Post" :: Text
      created = postCreated post
      lastTitle = Just $ postTitle post
      lastBody = Just $ postBody post
  ((res, formWidget), enctype) <- runFormPost $ newPostForm user created lastTitle lastBody bodyId
  case res of
    FormSuccess entry -> do
        runDB $ replace postId entry
        setMessage "Successfully edited post"
        redirect $ PostR postId
    _ -> defaultLayout $ do
               setTitle "New Post"
               $(widgetFile "new-post")

-- Form to create a new post
newPostForm :: UserId -> UTCTime -> Maybe Text -> Maybe Markdown -> Text -> Form Post
newPostForm user currentTime mtitle mbody bodyId = renderDivsNoLabels $ Post
  <$> areq textField titleFieldSettings mtitle
  <*> pure user
  <*> pure currentTime
  <*> (maybe "" id <$> (aopt markdownField bodyFieldSettings $ Just mbody))
    where
      bodyFieldSettings =
          FieldSettings { fsLabel = "Unused"
                        , fsTooltip = Nothing
                        , fsId = Just bodyId
                        , fsName = Nothing
                        , fsAttrs = [ ("placeholder", "Body")
                                    , ("class", "autoexpand autoexpand-big")
                                    , ("rows", "3")
                                    , ("data-min-rows", "3") ] }
      titleFieldSettings =
          FieldSettings { fsLabel = "Unused"
                        , fsTooltip = Nothing
                        , fsId = Nothing
                        , fsName = Nothing
                        , fsAttrs = [ ("placeholder", "Title") ] }

-- Form to create a new comment
newCommentForm :: PostId -> UserId -> Form Comment
newCommentForm postId user = renderDivsNoLabels $ Comment
  <$> areq textareaField messageFieldSettings Nothing
  <*> pure user
  <*> pure postId
    where
      messageFieldSettings =
          FieldSettings { fsLabel = "Unused"
                        , fsTooltip = Nothing
                        , fsId = Nothing
                        , fsName = Nothing
                        , fsAttrs = [ ("placeholder", "Comment...")
                                    , ("class", "autoexpand autoexpand-small")
                                    , ("rows", "3")
                                    , ("data-min-rows", "3") ] }

getPostsByR :: UserId -> Handler Html
getPostsByR userId = do
  author <- runDB $ get404 userId
  posts <- runDB $ selectList [ PostAuthor ==. userId ] [ Desc PostId ]
  let authorName = userDisplayName author
  defaultLayout $ do
    setTitle . toHtml $ authorName <> "'s Posts"
    $(widgetFile "posts-by")
