{-# LANGUAGE OverloadedStrings #-}
module Server (webServer) where

import Client
import Config
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Data.Default (def)
import Data.Function ((&))
import Data.List (foldl1')
import qualified Data.Map as Map
import Data.Streaming.Network.Internal (HostPreference(Host))
import Data.String.Conversions (cs)
import Text.XML.Light.Output (showTopElement)
import Web.Scotty(Options(settings), scottyOpts, get, html, raw, next, param, setHeader)
import Network.Wai.Handler.Warp (setPort, setHost)

-- |Convert 'ServerConfig' top 'Options'
getScottyOpts :: ServerConfig -> Options
getScottyOpts server = def {settings = def & settings & serverSettings} where
    serverSettings = foldl1' (.) [ setPort (server & port)
                                 , setHost (server & host & Host)
                                 ]

-- |Simple html representation of available feeds.
feedsList :: Foldable t => t (String, String) -> String
feedsList feeds = "<h2>Available feeds</h2><ul>" ++ concatMap (toLi . uncurry toA) feeds ++ "</ul>"

-- |Make html <li> element from string
toLi :: String -> String
toLi text = "<li>" ++ text ++ "</li>"

-- |Make html <a> element from link and text
toA :: String -> String -> String
toA link text = "<a href=\"" ++ link ++ "\">" ++ text ++ "</a>"

-- |Simple web server that shows list of available feeds on index page
-- and provides concatenation of configured feeds on appropriate URLs.
webServer :: Config -> IO ()
webServer config = do
    scottyOpts (getScottyOpts serverConfig) $ do
            get "/" $
                let
                    namesAndTitles = map (second title) (Map.assocs feedsConfig)
                in
                    html . cs . feedsList $ namesAndTitles
            
            get "/:feed" $ do
                feed <- param "feed"
                case Map.lookup feed feedsConfig of
                    Nothing -> next
                    Just feedConfig -> do
                        setHeader "Content-Type" "application/rss+xml"
                        feed <- liftIO $ concatFeeds feedConfig
                        raw $ cs . showTopElement $ feed

        where
            feedsConfig = feeds config
            serverConfig = server config
