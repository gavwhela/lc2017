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
getEditProfileR = postEditProfileR

postEditProfileR :: UserId -> Handler Html
postEditProfileR userId = do
  user <- runDB $ get404 userId
  bioId <- newIdent
  formElement <- newIdent
  ((res, formWidget), enctype) <- runFormPost $ editProfileForm user bioId
  case res of
    FormSuccess (name, bio) -> do
        runDB $ update userId [UserDisplayName =. name, UserBio =. bio]
        setMessage "Successfully updated profile"
        redirect $ ProfileR userId
    _ -> defaultLayout $ do
             setTitle . toHtml $ "Editing " <> userDisplayName user <> "'s Profile"
             $(widgetFile "edit-profile")

editProfileForm :: User -> Text -> Form (Text, Markdown)
editProfileForm user bioId = renderDivsNoLabels $ (,)
  <$> (areq textField nameFieldSettings $ Just $ userDisplayName user)
  <*> (fromMaybe "" <$> (aopt markdownField bioFieldSettings $ Just $ Just $ userBio user))
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
                        , fsAttrs = [ ("placeholder", "Name") ] }
