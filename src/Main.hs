module Main where

import qualified Data.Text.Lazy        as L
import qualified Data.Text.Lazy.IO     as LIO
import           Houndell
import           Houndell.Models
import           Houndell.Network.HTTP as HN
import           Houndell.UI.Simple
import           Options.Applicative

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

main :: IO ()
main = execParser opts >>= runHoundell
       where
         opts = info (helper <*> sample)
           (fullDesc
           <> progDesc ("Command line client for Hound, an extremely fast source code search engine.\n" ++
                        "Use -h or --help to print help.")
           <> header "houndell - hound command line client" )

runHoundell :: HoundellOptions -> IO ()
runHoundell opts = return opts >>= toHoundSettings >>= fetchResults >>= outputResult

toHoundSettings :: HoundellOptions -> IO HoundSettings
toHoundSettings opts = return $ HoundSettings (Main.url opts) (toHoundSearchParams opts)

toHoundSearchParams :: HoundellOptions -> HoundSearchParams
toHoundSearchParams opts = HoundSearchParams (Main.query opts) (Main.repos opts)

outputResult :: Maybe HoundResponse -> IO ()
outputResult (Just r) = LIO.putStrLn $ prettify r
outputResult Nothing = putStrLn "Nothing here, an error occurred??"
