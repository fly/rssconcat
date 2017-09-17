{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Server
import System.Environment (getArgs, getProgName)
import System.Exit

-- |Default configuration file is config.yaml in current working diretory.
defaultConfigFile :: String
defaultConfigFile = "config.yaml"

-- |Load config file and parse it into 'Config'
loadConfig :: [String] -> IO Config
loadConfig [] = getConfigFromFile defaultConfigFile
loadConfig [file] = getConfigFromFile file
loadConfig _ = do
    prog <- getProgName
    die $ "Usage: " ++ prog ++ " [config file]"

-- |Load config from file use it to start 'webServer'
main :: IO ()
main = do
    config <- loadConfig =<< getArgs
    webServer config
