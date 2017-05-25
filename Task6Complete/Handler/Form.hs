module Handler.Form where

import Import

data Post = Post
    { postTitle :: Text
    , postAuthor :: Text
    , postContent :: Textarea }

getFormR :: Handler Html
getFormR = do
  user <- requireAuthId
  (formWidget, enctype) <- generateFormPost $ postForm user
  defaultLayout $ do
        setTitle "Form"
        $(widgetFile "form")

postFormR :: Handler Html
postFormR = do
  user <- requireAuthId
  ((res, formWidget), enctype) <- runFormPost $ postForm user
  case res of
    FormSuccess post -> defaultLayout $ do
                          setTitle "Posted"
                          $(widgetFile "posted")
    _ -> defaultLayout $ do
              setTitle "Form"
              $(widgetFile "form")

postForm :: Text -> Form Post
postForm user = renderDivs $ Post
    <$> areq textField "Title" Nothing
    <*> pure user
    <*> areq textareaField "Contents" Nothing
