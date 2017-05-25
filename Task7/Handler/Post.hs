module Handler.Post where

import Import

data Post = Post
    { postTitle :: Text
    , postAuthor :: Text
    , postContent :: Textarea }

getNewPostR :: Handler Html
getNewPostR = do
  user <- requireAuthId
  (formWidget, enctype) <- generateFormPost $ postForm user
  defaultLayout $ do
        setTitle "New Post"
        $(widgetFile "new-post")

postNewPostR :: Handler Html
postNewPostR = do
  user <- requireAuthId
  ((res, formWidget), enctype) <- runFormPost $ postForm user
  case res of
    FormSuccess post -> defaultLayout $ do
                          setTitle "Post"
                          $(widgetFile "post")
    _ -> defaultLayout $ do
              setTitle "New Post"
              $(widgetFile "new-post")

postForm :: Text -> Form Post
postForm user = renderDivs $ Post
    <$> areq textField "Title" Nothing
    <*> pure user
    <*> areq textareaField "Contents" Nothing
