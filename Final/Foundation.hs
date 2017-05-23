{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)

import Yesod.Auth.Account
-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    }

-- Generates types and rendering definitions for routing declared in config/routes
--
-- mkYesodDispatch in Application.hs generates the dispatch for these
-- routes, so that the handlers don't have to be in scope here.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- There are better ways to do this, the full scaffolding has a
-- callback system, but this is a bit simpler.
menuItemsFor :: Maybe UserId -> [MenuItem]
menuItemsFor Nothing =
    [ MenuItem
      { menuItemLabel = "Blog"
      , menuItemRoute = HomeR }
    , MenuItem
      { menuItemLabel = "Login"
      , menuItemRoute = AuthR LoginR } ]
menuItemsFor (Just user) =
    [ MenuItem
      { menuItemLabel = "Blog"
      , menuItemRoute = HomeR }
    , MenuItem
      { menuItemLabel = "Profile"
      , menuItemRoute = ProfileR user }
    , MenuItem
      { menuItemLabel = "My Posts"
      , menuItemRoute = PostsByR user }
    , MenuItem
      { menuItemLabel = "New Post"
      , menuItemRoute = NewPostR }
    , MenuItem
      { menuItemLabel = "Logout"
      , menuItemRoute = AuthR LogoutR }
    ]

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthId
        mcurrentRoute <- getCurrentRoute

        -- Define the menu items of the header.
        let menuItems = menuItemsFor muser

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        pc <- widgetToPageContent $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized (PostR _) False = return Authorized
    isAuthorized (PostR _) True = isAuthenticated
    isAuthorized (EditPostR pid) _ = ownsResource pid postAuthor
    isAuthorized (PostsByR _) _ = return Authorized
    isAuthorized (CommentR cid) _ = ownsResource cid commentUserId
    isAuthorized PreviewR _ = isAuthenticated
    isAuthorized NewPostR _ = isAuthenticated
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized

    isAuthorized (ProfileR _) False = return Authorized
    isAuthorized (ProfileR uid) True = isUser uid
    isAuthorized (EditProfileR uid) _ = isUser uid

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

    -- Provide proper styling for default displays, like error pages
    defaultMessageWidget title body = $(widgetFile "default-message-widget")

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance PersistUserCredentials User where
  userUsernameF = UserUsername
  userPasswordHashF = UserPassword
  userEmailF = UserEmailAddress
  userEmailVerifiedF = UserVerified
  userEmailVerifyKeyF = UserVerifyKey
  userResetPwdKeyF = UserResetPasswordKey
  uniqueUsername = UniqueUser

  userCreate name email key pwd = User name pwd email name "" False key ""

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate creds = runDB $ do
        x <- insertBy $ User (credsIdent creds) "" "" (credsIdent creds) "" False "" ""
        case x of
            Left (Entity uid _) -> return $ Authenticated uid
            Right uid -> return $ Authenticated uid

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins app = [accountPlugin] ++ extraAuthPlugins
        -- Enable authDummy login if enabled.
        where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

    authHttpManager = error "No manager needed"

instance AccountSendEmail App

instance YesodAuthAccount (AccountPersistDB App User) App where
  runAccountDB = runAccountPersistDB

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

isUser :: UserId -> Handler AuthResult
isUser uid =
    do muserId <- maybeAuthId
       if maybe False ((==) uid) muserId
         then return Authorized
         else return $ Unauthorized "You do not own that resource"


-- | Check if the current user owns a db resource
ownsResource :: ( PersistEntity a
                , PersistStore (PersistEntityBackend a)
                , PersistEntityBackend a ~ YesodPersistBackend App) =>
                Key a -> (a -> (AuthId App)) -> HandlerT App IO AuthResult
ownsResource resourceId fromResource = do
  uid <- requireAuthId
  resource <- runDB $ get404 resourceId
  if fromResource resource == uid
    then return Authorized
    else return $ Unauthorized "You do not own that resource"

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
