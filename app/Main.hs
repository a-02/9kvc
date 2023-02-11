{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Text as Text
import Lucid
import Network.Wai.Handler.Warp (run)
import Web.Scotty

db_uh :: IO ()
db_uh = do
  conn <- open "gematria.db"
  close conn

main :: IO ()
main = do
  scottyApp app >>= run 23456 

app :: ScottyM ()
app = do
  get "/" do 
    html $ renderText pageContent

-- retrieve phrase from db on page load, not "when clicked"

pageContent :: Html ()
pageContent = do
  doctypehtml_ $ do 
    style_ $ Text.pack $ unlines
      [ "@font-face {"
      , "  font-family: \"Manrope\";"
      , "  src: local(\"Manrope\"), url(\"Manrope-ExtraBold.woff2\"), format(\"woff\");"
      , "}"
      , "body {"
      , "  overflow: hidden;"
      , "  position: absolute;"
      , "  width: 100%;"
      , "  height: 100%;"
      , "  display: flex;"
      , "  justify-content: center;"
      , "  align-items: center;"
      , "  flex-direction: column;"
      , "  margin: 0;"
      , "  font-size: 300%;"
      , "  font-family: \"Manrope\";"
      , "}"
      , "p {"
      , "  margin: 0px 0px 20px 0px;"
      , "}"
      ]
    head_ $ do
      title_ "help"
    body_ $ do 
      p_ "You're probably lost."
      p_ "Go away."
      a_ [href_ "twitter.com/nikshalark"] "@nikshalark"
