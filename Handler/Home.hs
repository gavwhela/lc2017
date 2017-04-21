module Handler.Home where

import Import
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))

getHomeR :: Handler Html
getHomeR = do
  -- Fetch list of posts for homepage
  posts <- runDB $
             E.select $
             E.from $ \(post `E.InnerJoin` user) -> do
                E.orderBy [E.desc $ post ^. PostId]
                E.on $ post ^. PostAuthor E.==. user ^. UserId
                return ( post ^. PostId
                       , post ^. PostTitle
                       , user ^. UserUsername )
  defaultLayout $ do
        setTitle "Blog!"
        $(widgetFile "homepage")
