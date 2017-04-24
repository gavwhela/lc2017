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
import Handler.Helpers

getPostR :: PostId -> Handler Html
getPostR = postPostR

postPostR :: PostId -> Handler Html
postPostR postId = do
  muser <- maybeAuthId
  post <- runDB $ get404 postId
  poster <- runDB $ get404 $ postAuthor post
  let date = formatDate $ postCreated post
  comments <- runDB (getComments postId)
  let postHTML = markdownToHtml $ postBody post
  let isOwner = maybe False ((==) $ postAuthor post) muser
  mform <- traverse (runFormPost . newCommentForm postId) muser
  case mform of
    Nothing -> defaultLayout $ do
                     setTitle . toHtml $ postTitle post
                     $(widgetFile "post")
    Just ((FormSuccess entry, _), _) ->
        do runDB $ insert_ entry
           setMessage "Successfully created comment"
           redirect (PostR postId)
    Just _ ->
        defaultLayout $ do
               setTitle . toHtml $ postTitle post
               $(widgetFile "post")

deletePostR :: PostId -> Handler Html
deletePostR postId = do
  userId <- requireAuthId
  post <- runDB $ get404 postId
  unless (userId == postAuthor post) (permissionDenied "You do not own that post")
  runDB $ do
    deleteWhere [CommentPostId ==. postId]
    delete postId
  setMessage "Deleted Post"
  redirect HomeR

getComments :: Key Post -> ReaderT SqlBackend Handler [( E.Value Textarea
                                                       , E.Value CommentId
                                                       , E.Value Text
                                                       , E.Value UserId)]
getComments postId = E.select $
     E.from $ \(comment `E.InnerJoin` user) -> do
        E.where_ ( comment ^. CommentPostId E.==. E.val postId )
        E.orderBy [E.desc $ comment ^. CommentId]
        E.on $ comment ^. CommentUserId E.==. user ^. UserId
        return ( comment ^. CommentMessage
               , comment ^. CommentId
               , user ^. UserDisplayName
               , user ^. UserId)

deleteCommentR :: CommentId -> Handler ()
deleteCommentR commentId = runDB $ delete commentId

parsePreview :: Value -> Parser Markdown
parsePreview = withObject "preview" (\obj -> do
                                       markdown <- obj .: "markdown"
                                       return $ Markdown markdown)

postPreviewR :: Handler Value
postPreviewR = do
  mMarkdown <- do rval <- parseCheckJsonBody
                  case rval of
                    Error _ -> return Nothing
                    Success val -> return $ parseMaybe parsePreview val
  case mMarkdown of
    Nothing -> return $ object []
    Just markdown ->
        let mpreviewHtml = markdownToHtml markdown
        in case mpreviewHtml of
             Left _ -> return $ object []
             Right previewHtml ->
               return $ object [ "html" .= (LE.decodeUtf8 $ renderHtml previewHtml)]

getNewPostR :: Handler Html
getNewPostR = postNewPostR

postNewPostR :: Handler Html
postNewPostR = do
  user <- requireAuthId
  time <- liftIO getCurrentTime
  bodyId <- newIdent
  formId <- newIdent
  let submitRoute = NewPostR
      header = "New Post" :: Text
  ((res, formWidget), enctype) <- runFormPost $ newPostForm user time Nothing "" bodyId
  case res of
    FormSuccess entry -> do
        runDB $ insert_ entry
        setMessage "Successfully created post"
        redirect $ PostsByR user
    _ -> defaultLayout $ do
               setTitle "New Post"
               $(widgetFile "new-post")

getEditPostR :: PostId -> Handler Html
getEditPostR = postEditPostR

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
      lastBody = postBody post
  ((res, formWidget), enctype) <- runFormPost $ newPostForm user created lastTitle lastBody bodyId
  case res of
    FormSuccess entry -> do
        runDB $ replace postId entry
        setMessage "Successfully edited post"
        redirect $ PostR postId
    _ -> defaultLayout $ do
               setTitle "Edit Post"
               $(widgetFile "new-post")

newPostForm :: UserId -> UTCTime -> Maybe Text -> Markdown -> Text -> Form Post
newPostForm user currentTime mtitle body bodyId = renderDivsNoLabels $ Post
  <$> areq textField titleFieldSettings mtitle
  <*> pure user
  <*> pure currentTime
  <*> areq markdownField bodyFieldSettings (Just body)
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
