module Handler.Posts where

import Import
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import Yesod.Markdown

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
  -- Generate HTML for markdown post
  let postHTML = markdownToHtml $ postBody post
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
        E.orderBy [E.desc $ comment ^. CommentId]
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
  -- Generate HTML for markdown post
  let postHTML = markdownToHtml $ postBody post
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
newPostForm :: UserId -> UTCTime -> Html -> MForm Handler (FormResult Post, Widget)
newPostForm user currentTime extra = do
  (titleRes, titleView) <- mreq textField titleFieldSettings Nothing
  (bodyRes, bodyView) <- mreq markdownField bodyFieldSettings Nothing
  let postRes = Post <$> titleRes <*> pure user <*> pure currentTime <*> bodyRes
  let widget = do
        toWidget
            [lucius|
               ##{fvId titleView} {
                   width: 100%;
                   margin-bottom: 10px;
               }
               ##{fvId bodyView} {
                   width: 100%;
                   resize: none;
                   border-radius: 3px;
               }
            |]
        [whamlet|
            #{extra}
            ^{fvInput titleView}
            ^{fvInput bodyView}
        |]
  return (postRes, widget)
    where
      bodyFieldSettings =
          FieldSettings { fsLabel = "Unused"
                        , fsTooltip = Nothing
                        , fsId = Nothing
                        , fsName = Nothing
                        , fsAttrs = [ ("placeholder", "Body")
                                    , ("class", "autoexpand")
                                    , ("rows", "3")
                                    , ("data-min-rows", "3") ] }
      titleFieldSettings =
          FieldSettings { fsLabel = "Unused"
                        , fsTooltip = Nothing
                        , fsId = Nothing
                        , fsName = Nothing
                        , fsAttrs = [ ("placeholder", "Title") ] }

-- Form to create a new comment
newCommentForm :: PostId -> UserId -> Html -> MForm Handler (FormResult Comment, Widget)
newCommentForm postId user extra = do
  (messageRes, messageView) <- mreq textareaField messageFieldSettings Nothing
  let commentRes = Comment <$> messageRes <*> pure user <*> pure postId
  let widget = do
        toWidget
            [lucius|
               ##{fvId messageView} {
                   width: 100%;
                   resize: none;
                   border-radius: 3px;
               }
            |]
        [whamlet|
            #{extra}
            ^{fvInput messageView}
        |]
  return (commentRes, widget)
    where
      messageFieldSettings =
          FieldSettings { fsLabel = "Unused"
                        , fsTooltip = Nothing
                        , fsId = Nothing
                        , fsName = Nothing
                        , fsAttrs = [ ("placeholder", "Comment...")
                                    , ("class", "autoexpand")
                                    , ("rows", "3")
                                    , ("data-min-rows", "3") ] }
