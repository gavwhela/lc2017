{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import Yesod

-- Foundation datatype, we'll talk about this later
data Links = Links

mkYesod "Links" [parseRoutes|
/      HomeR  GET
/page1 Page1R GET
/page2 Page2R GET
/page3 Page3R GET
|]

instance Yesod Links

getHomeR, getPage1R, getPage2R, getPage3R :: Handler Html
getHomeR  = defaultLayout [whamlet|<a href=@{Page1R}>Go to page 1!|]
getPage1R = defaultLayout [whamlet|<a href=@{Page2R}>Go to page 2!|]
getPage2R = defaultLayout [whamlet|<a href=@{Page3R}>Go to page 3!|]
getPage3R = defaultLayout [whamlet|<a href=@{HomeR}>Go home!|]

main :: IO ()
main = warp 3000 Links
