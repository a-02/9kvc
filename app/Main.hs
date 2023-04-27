{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class
import Lucid
import Web.Scotty
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static

import Content

main :: IO ()
main = do
  scottyApp app >>= run 1999

--  execute conn
--    "INSERT INTO tbl (id, fortune, time, address) VALUES (?,?,?,?)" (Fortune 1 "Hello." 1 "what")

-- Conveniench.
io :: MonadIO m => IO a -> m a
io = liftIO

app :: ScottyM ()
app = do
  middleware static
  get "/" do 
    html $ renderText homeContent
  get "/about" do
    html $ renderText aboutContent
  get "/email" do
    html $ renderText emailContent
  get "/text" do
    html $ renderText textContent
  get "/secret" do
    html $ renderText secretContent
