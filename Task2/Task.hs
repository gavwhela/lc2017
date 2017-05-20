{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import Yesod

data Task = Task

mkYesod "Task" [parseRoutes|
/      HomeR  GET
|]

instance Yesod Task

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
             setTitle "Welcome Home"
             addScriptRemote "https://code.jquery.com/jquery-3.2.1.js"

             toWidget [lucius| h1 { color: green } |]

             toWidget
               [julius|
                 $("h1").click(function(){
                     alert("Header Clicked");
                 });
               |]

             toWidgetHead
               [hamlet|
                 <meta name=description content=#{contentDescription}>
               |]

             [whamlet|
                <h1>Welcome Back!
                ^{footer copyright}
             |]


    where contentDescription = "The homepage" :: String
          copyright = "Someone" :: String

-- Add a footer here that displays the given copyright string and uses
-- lucius to add style, such as changing the color of the text or
-- adding a background attribute
footer :: String -> Widget
footer copyright = mempty

-- After you have completed the exercise, please complete the
-- assessment in the guide. At this point a good follow up is to add
-- an additional page to the application, reusing the footer you have
-- written.

main :: IO ()
main = warp 3000 Task
