module Handler.Profile where

import Import
import Yesod.Auth.Account (resetPasswordR)
import Yesod.Markdown
import Text.Julius (rawJS)

getProfileR :: UserId -> Handler Html
getProfileR userId = do
    viewerId <- maybeAuthId
    user <- runDB $ get404 userId
    let isOwner = maybe False ((==) userId) viewerId
    let bioHTML = markdownToHtml $ userBio user
    csrfToken <- fmap reqToken getRequest
    defaultLayout $ do
        setTitle . toHtml $ userDisplayName user <> "'s Profile"
        $(widgetFile "profile")

getEditProfileR :: UserId -> Handler Html
getEditProfileR userId = do
  user <- runDB $ get404 userId
  previewButton <- newIdent
  previewDiv <- newIdent
  bioId <- newIdent
  (formWidget, enctype) <- generateFormPost $ editProfileForm user bioId
  defaultLayout $ do
    setTitle . toHtml $ "Editing " <> userDisplayName user <> "'s Profile"
    $(widgetFile "edit-profile")

postEditProfileR :: UserId -> Handler Html
postEditProfileR userId = do
  user <- runDB $ get404 userId
  previewButton <- newIdent
  previewDiv <- newIdent
  bioId <- newIdent
  ((res, formWidget), enctype) <- runFormPost $ editProfileForm user bioId
  case res of
    FormSuccess (name, bio) -> do
        runDB $ update userId [UserDisplayName =. name, UserBio =. bio]
        setMessage "Successfully updated profile"
        redirect $ ProfileR userId
    _ -> defaultLayout $ do
             setTitle . toHtml $ "Editing " <> userDisplayName user <> "'s Profile"
             $(widgetFile "edit-profile")

editProfileForm :: User -> Text -> Html -> MForm Handler (FormResult (Text, Markdown), Widget)
editProfileForm user bioId extra = do
  (nameRes, nameView) <- mreq textField nameFieldSettings $ Just $ userDisplayName user
  (bioRes, bioView) <- mreq markdownField bioFieldSettings $ Just $ userBio user
  let profileRes = (,) <$> nameRes <*> bioRes
  let widget = do
        toWidget
            [lucius|
               ##{fvId nameView} {
                   width: 100%;
                   margin-bottom: 10px;
               }
               ##{fvId bioView} {
                   width: 100%;
                   resize: none;
                   border-radius: 3px;
               }
            |]
        [whamlet|
            #{extra}
            ^{fvInput nameView}
            ^{fvInput bioView}
        |]
  return (profileRes, widget)
    where
      bioFieldSettings =
          FieldSettings { fsLabel = "Unused"
                        , fsTooltip = Nothing
                        , fsId = Just bioId
                        , fsName = Nothing
                        , fsAttrs = [ ("placeholder", "Bio")
                                    , ("class", "autoexpand autoexpand-big")
                                    , ("rows", "3")
                                    , ("data-min-rows", "3") ] }
      nameFieldSettings =
          FieldSettings { fsLabel = "Unused"
                        , fsTooltip = Nothing
                        , fsId = Nothing
                        , fsName = Nothing
                        , fsAttrs = [ ("placeholder", "name") ] }
