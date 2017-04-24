module Handler.Helpers where

import Import

-- Format a UTCTime into a human readable date
formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale dateFormat
    where dateFormat = "%a, %B %e, %0Y"
