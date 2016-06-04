{-# LANGUAGE OverloadedStrings #-}

module Houndell.UI.Simple (prettify) where

import           Data.Map              (assocs)
import           Data.Text.Lazy        as L (Text (..), append, unlines)
import           Formatting
import           Formatting.Formatters
import           Houndell.Models
import           Text.Printf

class Prettify a where
    prettify :: a -> Text

instance Prettify Match where
    prettify m = L.unlines $ zipWith formatLine (lineNumbers m) (codeSnippet m)

formatLine :: Int -> Text -> Text
formatLine = format ((left 5 ' ' %.int)% "| " % text)

lineNumbers :: Match -> [Int]
lineNumbers m = [start..end]
                where
                  ln = lineNumber m
                  start = ln - length (before m)
                  end = ln + length (after m)

codeSnippet :: Match -> [Text]
codeSnippet m = before m ++ line m : after m

instance Prettify FileMatches where
    prettify m = "--> " `append` filename m `append` "\n" `append` L.unlines (fmap prettify (matches m))

instance Prettify RepoMatches where
    prettify m = L.unlines $ fmap prettify (files m)

formatRepos :: (Text, RepoMatches) -> Text
formatRepos (a, b) = "==> " `append` a `append` "\n" `append` prettify b

instance Prettify HoundResponse where
    prettify r = format("[files: " % int % ", time: " % int % "ms]\n" % text) files time rslt
                 where
                   st = stats r
                   files = filesOpened st
                   time = duration st
                   rslt = L.unlines (map formatRepos (assocs $ results r))

