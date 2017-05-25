{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

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
/                HomeR  GET
/binop/#Int/#Int BinopR GET
|]

instance Yesod Task

-- To get the function, use:
-- binOp <- fmap taskBinOp getYesod

-- For testing you can add a link with specific numbers, or enter the
-- url manually. For a link you can do something like:
-- <a href=@{BinopR 4 5}>Test with four and five
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
             setTitle "Welcome Home"
             [whamlet|
               <h1>Welcome Back!
               <a href=@{BinopR 4 5}>Test with four and five
             |]

getBinopR :: Int -> Int -> Handler Html
getBinopR x y = do
  binop <- fmap taskBinOp getYesod
  let res = binop x y
  defaultLayout $ do
    setTitle "Binop Page"
    [whamlet|<p>The Result: #{res}|]

-- Try changing the binary operation to -, or another numerical binop.
main :: IO ()
main = warp 3000 ( Task { taskBinOp = (-) } )
