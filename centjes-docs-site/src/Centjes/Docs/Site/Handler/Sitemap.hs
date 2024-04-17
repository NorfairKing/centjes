module Centjes.Docs.Site.Handler.Sitemap
  ( getRobotsR,
    getSitemapR,
  )
where

import Centjes.Docs.Site.Foundation
import qualified Data.Map as M
import Data.Text (Text)
import Language.Haskell.TH.Load
import Yesod.Sitemap

getRobotsR :: Handler Text
getRobotsR = robots RobotsR

getSitemapR :: Handler TypedContent
getSitemapR = do
  urls <- loadIO getUrls
  sitemapList urls

getUrls :: Load [SitemapUrl (Route App)]
getUrls =
  ( \dps ->
      concat
        [ [ SitemapUrl
              { sitemapLoc = HomeR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Weekly,
                sitemapPriority = Just 0.9
              },
            SitemapUrl
              { sitemapLoc = CentjesR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Monthly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = CentjesImportRevolutR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Monthly,
                sitemapPriority = Just 0.5
              }
          ],
          map
            ( \(urlPieces, _) ->
                SitemapUrl
                  { sitemapLoc = PageR urlPieces,
                    sitemapLastMod = Nothing,
                    sitemapChangeFreq = Just Yearly,
                    sitemapPriority = Just 0.3
                  }
            )
            (M.toList dps)
        ]
  )
    <$> docPages
