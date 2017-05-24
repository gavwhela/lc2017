{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import Yesod

-- For this exercise your task is to add a new page to this
-- application, which takes two Int's in as dynamic path components,
-- and performs the binary operation stored in the foundation datatype
-- and displays the result.

-- After you have completed the exercise, please complete the
-- assessment in the guide. At this point a good follow up is to add
-- an additional page to the application and use it to test out a
-- redirect, or one of the error functions (notFound, badMethod,
-- permissionDenied, notAuthenticated).

data Task = Task { taskBinOp :: Int -> Int -> Int }

-- Remember dynamic path components are prefixed with a #
mkYesod "Task" [parseRoutes|
/      HomeR  GET
|]

instance Yesod Task

-- To get the function, use:
-- binOp <- fmap taskBinOp getYesod

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
             setTitle "Welcome Home"
             [whamlet|<h1>Welcome Back!|]

-- Try  changing the binary operation to -, or another numerical binop.
main :: IO ()
main = warp 3000 ( Task { taskBinOp = (+) } )
