{-# LANGUAGE OverloadedStrings #-}

module Client (concatFeeds) where

import Config
import Control.Concurrent.Async (mapConcurrently)
import Data.Function (on, (&))
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody)
import Text.Feed.Constructor (newFeed, FeedKind(RSSKind), withFeedItems, withFeedTitle, withFeedDescription)
import Text.Feed.Export (xmlFeed)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query (feedItems, getItemId)
import Text.Feed.Types (Feed, Item)
import Text.RSS.Syntax(RSSItem(..))
import Text.XML.Light (add_attr)
import Text.XML.Light.Types (Attr(..), QName(..), Element)

-- |Eq is needed for Ord
instance Eq Item where
    (==) = (==) `on` getItemId

-- |Make instance of Ord, so 'Item' can be used as 'Set' element
instance Ord Item where
    compare = compare `on` getItemId

-- |Download given url and parse it as feed.
downloadFeed :: String -> IO (Maybe Feed)
downloadFeed url = do
    resp <- parseRequest url >>= httpLBS
    return $ parseFeedSource . getResponseBody $ resp

-- |Download feeds from all urls and collect their items into 'Set'
allFeedsItems :: [String] -> IO (Set Item)
allFeedsItems urls = do
    feeds <- catMaybes <$> mapConcurrently downloadFeed urls
    return $ Set.fromList . concatMap feedItems $ feeds


-- |Concat all feeds from feedConfig into new RSS 2.0 feed and return it as Element
concatFeeds :: FeedConfig -> IO Element
concatFeeds feedConfig = do
        feedItems <- allFeedsItems (urls feedConfig)
        let 
            feed = newFeed (RSSKind Nothing)
                & (withFeedItems (Set.toList feedItems))
                & (withFeedTitle (title feedConfig))
                & (withFeedDescription (title feedConfig))
        return $
            xmlFeed feed
            & (add_attr atomNs)  -- Some RSS feeds use this namespace. E.g.: stackoverflow

    where 
        atomNs = Attr { attrKey = QName { qName = "a10"
                                        , qURI = Nothing
                                        , qPrefix = Just "xmlns"
                                        }
                      , attrVal = "http://www.w3.org/2005/Atom"
                      }
