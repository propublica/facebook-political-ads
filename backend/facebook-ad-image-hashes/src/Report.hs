{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Report where


import           Data.List                       (groupBy, scanl', sort,
                                                  sortBy)
import           Data.Semigroup                  ((<>))
import qualified Data.Text                       as T
import           Data.Foldable                   (for_)
import           Data.Traversable                (for)
import           Text.Blaze.Html.Renderer.Pretty
import           Text.Blaze.Html5                hiding (head, map)
import qualified Text.Blaze.Html5                as HTML
import qualified Text.Blaze.Html5                hiding (head, map)
import           Text.Blaze.Html5.Attributes     hiding (for, title)
import           Prelude                         hiding (div)
import           Data.PHash                      (PHash(..), hammingDistance,
                                                  imageHash)

import           Search

------------------------------------------------------------------------------
-- | Sketch up an html page for the results of various types of searches
htmlReport :: [Either T.Text SearchResults] -> String
htmlReport res = renderHtml $ html $ do
  HTML.head $ do
    myHead
  body $ do
    for_ res $ \r -> div ! class_ "query" $ case r of
      Left err -> div $ p (toMarkup err)
      Right (NearestResult (inp, qHash) (PHash rHash, rURLs)) -> do
        div ! class_ "q-input" $ inputPath inp
        div ! class_ "result-header" $ do
          toMarkup rHash
          toMarkup ("Distance: " ++ show (hammingDistance qHash (PHash rHash)))
          for_ rURLs (\(URL u) -> img ! src (toValue u))
      Right (KNearestResult (inp, qHash) rs) -> do
        div ! class_ "entries" $ do
          div $ do
            h3 "Query"
            div ! class_ "results" $ hashEntry [inp] (qHash) Nothing
          div $ do
            h3 "Results"
            div ! class_ "results" $ for_ rs $ \(rHash, rUrls) ->
              hashEntry (Right <$> rUrls)
                        rHash
                        (Just $ hammingDistance qHash rHash)
      Right (NearestInRanges (inp, qHash) ranges rings) -> do
        div $ do
          h3 "Query"
          div ! class_ "results" $ hashEntry [inp] (qHash) Nothing
        div $ do
          h3 "Results"
          let rangePairs = zip (zip (0:ranges) ranges) rings
          for_ rangePairs $ \(dRange, ringResults) ->
            div ! class_ "radius-range" $ do
              distanceRange dRange
              div ! class_ "results" $ for_ ringResults $ \(rHash, rUrls) ->
                  hashEntry (Right <$> rUrls)
                            rHash
                            (Just $ hammingDistance qHash rHash)


------------------------------------------------------------------------------
-- | Render a filepath or url to Html in our report
--   Filepaths are just printed as text. Urls produce thumbnail-sized images
inputPath :: Either FilePath URL -> Html
inputPath (Left fp)         = toMarkup $ "Local file: " <> fp
inputPath (Right (URL url)) = img ! src (toValue url)


------------------------------------------------------------------------------
-- | Render a distance range to html
distanceRange :: (Int, Int) -> Html
distanceRange (r0, r1) =
  div ! class_ "radius-label" $
  toMarkup ("( " ++ show r0 ++ " - " ++ show r1 ++ " )")


------------------------------------------------------------------------------
-- | Render a query or result image card, optionally with hamming distance
hashEntry :: [Either FilePath URL] -> PHash -> Maybe Int -> Html
hashEntry urls (PHash hash) mDistance =
  div ! class_ "result-entry" $ do
    div ! class_ "result-header" $ do
      div . toMarkup $ hash
      maybe mempty (div . toMarkup . ("Distance: " <>) . show) mDistance
    div ! class_ "result-pics" $ do
      for_ (take 1 urls) $ \inp ->
        let (URL url) = assumeUrl "Render hash entry image" inp
        in  a ! href (toValue url) $
            (img ! src (toValue url) ! class_ "result-image")
    div ! class_ "result-links" $ for_ (zip [1..] urls) $ \(i, inp) ->
      let (URL url) = assumeUrl "Render hash entry link" inp
      in  a ! href (toValue url) $ toMarkup (i :: Int)
  where

    assumeUrl :: String -> Either FilePath URL -> URL
    assumeUrl errorHelper (Left fp) =
      error $ errorHelper ++
      "\nViolated expectation that an input was a URL: " ++ fp
    assumeUrl _ (Right url) = url


------------------------------------------------------------------------------
-- | Define some inline css
myHead :: Html
myHead = HTML.style . toMarkup $ unlines [
    "body {"
  , " background-color: hsl(0,0%,90%);"
  , "}"
  , ""
  , ".query {"
  , " display: flex;"
  , " flex-direction: column;"
  , " margin: 20px;"
  , " padding: 20px;"
  , " background-color: white;"
  , "}"
  , ""
  , ".result-image, .query-image {"
  , " height: 200px;"
  , " margin: 0px;"
  , "}"
  , ""
  , ".result-entry {"
  , " display: flex;"
  , " flex-direction: column;"
  , " background-color: white;"
  , " margin-right:  5px;"
  , " margin-bottom: 5px;"
  , "}"
  , ""
  , ".result-header, .query-header {"
  , " display: flex;"
  , " background-color: white;"
  , "}"
  , ""
  , ".result-header > div, .query-header > div {"
  , " margin: 3px;"
  , " font-size: 16pt;"
  , "}"
  , ""
  , ".result-pics {"
  , " display: flex;"
  , " x-overflow: scroll;"
  , " background-color: white;"
  , "}"
  , ""
  , ".result-links > a {"
  , " margin: 3 px;"
  , " text-decoration-line: none;"
  , "}"
  , ""
  , ".results {"
  , " display: flex;"
  , " flex-direction: row;"
  , " flex-wrap: wrap;"
  , " margin: 20px;"
  , " background-color: white;"
  , "}"
  , ".radius-range {"
  , " display: flex;"
  , " flex-direction: row;"
  , " flex-wrap: wrap;"
  , " align-items: center;"
  , " margin: 0px;"
  , "}"
  ]

-- Wireframe to code against
--
-- Query
-- | 12345678
-- | |-----|
-- | |     |
-- | |-----|
--
-- Results
-- | 12345679  Distance: 20
-- | |-----|
-- | |     |
-- | |-----|
