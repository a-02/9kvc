{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class
import Data.List (sort)
import Lucid
import Web.Scotty
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import System.Directory

import Content

-- todo: make this a fold
pairs :: Monoid a => [a] -> [(a,a)]
pairs (a:b:xs) = (a,b):(pairs xs)
pairs [a] = [(a, mempty)]
pairs [] = []

main :: IO ()
main = do
  tunes <- listDirectory "tunes"
  texts <- listDirectory "texts"
  -- This isn't as pretty as I'd like.
  let (as,bs) = unzip . pairs . sort $ tunes
  bs' <- traverse readFile $ fmap (\a -> "tunes/" ++ a) bs
  let tunes' = zip as bs'
  _ <- traverse print tunes'
  _ <- traverse print texts
  scottyApp (app (tunes',texts)) >>= run 1999

type Tunes = [(FilePath,String)]
type Texts = [FilePath]

--  execute conn
--    "INSERT INTO tbl (id, fortune, time, address) VALUES (?,?,?,?)" (Fortune 1 "Hello." 1 "what")

-- Conveniench.
io :: MonadIO m => IO a -> m a
io = liftIO

app :: (Tunes,Texts) -> ScottyM ()
app (tunes,texts) = do
  middleware static
  get "/" do 
    html $ renderText homeContent
  get "/tunes" do
    html $ renderText (tunesContent tunes)
  get "/text" do
    html $ renderText (textContent texts)
  get "/secret" do
    html $ renderText secretContent
