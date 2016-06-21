{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.Lazy        as L
import qualified Data.Text.Lazy.IO     as LIO
import           Houndell
import           Houndell.Models
import           Houndell.Network.HTTP as HN
import           Houndell.UI.Simple
import qualified Network.HTTP.Client   as HTTP
import           Options.Applicative
import qualified Control.Exception.Base as CE

data HoundellMode = Simple|UI

parseHoundellMode :: Monad m => String -> m HoundellMode
parseHoundellMode "simple" = return Simple
parseHoundellMode "ui" = return UI
parseHoundellMode _ = fail "Must be one of [simple|ui]"

data HoundellOptions = HoundellOptions {
    url   :: String,
    query :: String,
    repos :: Maybe String,
    mode  :: Maybe HoundellMode,
    debug :: Bool
}

sample :: Parser HoundellOptions
sample = HoundellOptions
    <$> strOption(
        long "url"
        <> short 'u'
        <> metavar "URL"
        <> help "Base url. [required]")
    <*> strOption (
        long "query"
        <> short 'q'
        <> metavar "QUERY"
        <> help "Query to search hound. [required]")
    <*> optional(strOption (
        long "repos"
        <> short 'r'
        <> metavar "REPO1,REPO2,..."
        <> help "Comma separated repositories. "))
    <*> optional (option(str >>= parseHoundellMode) (
        long "mode"
        <> short 'm'
        <> metavar "simple|ui"
        <> value Simple
        <> help "Houndell mode. [default: simple] [not yet implemented]."))
    <*> switch (
        long "debug"
        <> short 'd'
        <> help "Print debugging info.")

errorHandler :: HTTP.HttpException -> IO ()
errorHandler ex = putStrLn $ "Error: " ++ show ex

main :: IO ()
main = do
         houndelOptions <- execParser opts
         CE.catch (runHoundell houndelOptions) errorHandler
         where
           opts = info (helper <*> sample)
             (fullDesc
             <> progDesc ("Command line client for Hound, an extremely fast source code search engine.\n" ++
                          "Use -h or --help to print help.")
             <> header "houndell - hound command line client written in Haskell" )

runHoundell :: HoundellOptions -> IO ()
runHoundell opts = fetchResults (toHoundSettings opts) >>= printResult opts

toHoundSettings :: HoundellOptions -> HoundSettings
toHoundSettings opts = HoundSettings (Main.url opts) houndSearchParams
                       where
                         houndSearchParams = HoundSearchParams (Main.query opts) (Main.repos opts)

printResult :: HoundellOptions -> Maybe HoundResponse -> IO ()
printResult opts (Just r) = LIO.putStrLn $ Main.results opts r
printResult _ Nothing = putStrLn "Unknown error occurred."

results :: HoundellOptions -> HoundResponse -> L.Text
results opts r = debugOutput (debug opts) (toRequest (toHoundSettings opts)) `L.append` prettify r

debugOutput :: Bool -> Maybe HTTP.Request -> L.Text
debugOutput True (Just r) = "DEBUG: Request URL: " `L.append` L.pack (show r)
debugOutput _ _ = ""
