{-# LANGUAGE OverloadedStrings #-}
module Config (
    FeedConfig(..),
    Feeds,
    ServerConfig(..),
    Config(..),
    getConfigFromFile
    ) where


import Data.Function ((&))
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Yaml.Syck as Syck
import Text.Read (readMaybe)

-- |Configuration for a single feed
data FeedConfig = FeedConfig
    { title :: String
    , urls :: [String]
    }
    deriving Show

-- |All feeds is a Map where key is feed name (and url endpoint) and value is 'FeedConfig'
type Feeds = Map String FeedConfig

-- |Configuration for web server
data ServerConfig = ServerConfig
    { host :: String
    , port :: Int
    }
    deriving Show

-- |Total application configuration
data Config = Config
    { server :: ServerConfig
    , feeds :: Feeds
    }
    deriving Show

-- |Throws simple error
wrongConfig :: a
wrongConfig = error "wrong config"

-- |Default host for web server
defaultHost :: String
defaultHost = "127.0.0.1"

-- |default port for web server
defaultPort :: Int
defaultPort = 8080

-- |Extract string from 'EStr'
extractStr :: YamlElem -> String
extractStr (EStr s) = unpackBuf s

-- |Extract int from 'EStr'
extractInt :: YamlElem -> Int
extractInt elem = case readMaybe (extractStr elem) of
    Just port -> port
    Nothing -> wrongConfig

-- |Extract list of 'YamlNode' from 'ESeq'
extractSeq :: YamlElem -> [YamlNode]
extractSeq (ESeq seq) = seq

-- |Generate server config from appropriate 'YamlElem'
getServerConfig :: YamlElem -> ServerConfig
getServerConfig (EMap ns) = foldl' updateConfig newConfig ns where
    newConfig = ServerConfig {host=defaultHost, port=defaultPort}
    updateConfig cfg (nodeK, nodeV) = case n_elem nodeK of
        EStr "host" -> cfg {host = (nodeV & n_elem & extractStr)}
        EStr "port" -> cfg {port = (nodeV & n_elem & extractInt)}

-- |Generate single feed config from appropriate 'YamlElem'
getFeedConfig :: YamlElem -> FeedConfig
getFeedConfig (EMap ns) = foldl' updateConfig newConfig ns where
    newConfig = FeedConfig {title=undefined, urls=undefined}  -- Not sure if I like this.
    updateConfig cfg (nodeK, nodeV) = case n_elem nodeK of
        EStr "title" -> cfg {title = extractStr (n_elem nodeV)}
        EStr "urls" -> cfg {urls = feedUrls} where
            feedUrls = map (extractStr . n_elem) (extractSeq . n_elem $ nodeV)

-- |Generate config for all feeds from appropriate 'YamlElem'
getFeeds :: YamlElem -> Feeds
getFeeds (EMap ns) = Map.fromList $ map getFeed ns where
    getFeed (nodeK, nodeV) =
        ( extractStr . n_elem $ nodeK
        , getFeedConfig . n_elem $ nodeV
        )

-- |Generate application config from 'YamlNode' of config file
configFromYaml :: YamlNode -> Config
configFromYaml node = foldl' updateConfig newConfig (nodes (n_elem node)) where
    newConfig = Config { server = ServerConfig{host=defaultHost, port=defaultPort}
                       , feeds = Map.empty}
    nodes (EMap ns) = ns
    nodes _ = wrongConfig
    updateConfig cfg (nodeK, nodeV) = case n_elem nodeK of
        EStr "server" -> cfg {server = (nodeV & n_elem & getServerConfig)}
        EStr "feeds" -> cfg {feeds = (nodeV & n_elem & getFeeds)}


-- |Parse yaml file to 'Config'
getConfigFromFile :: String -> IO Config
getConfigFromFile file = configFromYaml <$> Syck.parseYamlFile file
