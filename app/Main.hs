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
import Network.Wai.Parse
import System.Directory
import System.FilePath ((</>))
import System.Random
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Char8 qualified as BSC8

import Content
import Data.Foldable

-- todo: make this a fold
pairs :: Monoid a => [a] -> [(a,a)]
pairs (a:b:xs) = (a,b):pairs xs
pairs [a] = [(a, mempty)]
pairs [] = []

main :: IO ()
main = do
  tunes <- listDirectory "tunes"
  texts <- listDirectory "texts"
  pics <- listDirectory "art"
  vids <- listDirectory "video"
  -- This isn't as pretty as I'd like.
  let (as,bs) = unzip . pairs . sort $ tunes
  bs' <- traverse (readFile . ("tunes/" ++)) bs
  let tunes' = zip as bs'
  i <- randomRIO (0, 4)
  print "spinnin' ..."
  scottyApp (app i (tunes',texts,vids,pics)) >>= run 80

type Tunes = [(FilePath,String)]
type Texts = [FilePath]
type Vids = [FilePath]
type Pics = [FilePath]

--  execute conn
--    "INSERT INTO tbl (id, fortune, time, address) VALUES (?,?,?,?)" (Fortune 1 "Hello." 1 "what")

-- Conveniench.
io :: MonadIO m => IO a -> m a
io = liftIO

app :: Int -> (Tunes,Texts,Vids,Pics) -> ScottyM ()
app i (tunes,texts,vids,pics) = do
  middleware static
  get "/" do
    html $ renderText (homeContent i)
  get "/tunes" do
    html $ renderText (tunesContent tunes)
  get "/text" do
    html $ renderText (textContent texts)
  get "/secret" do
    html $ renderText (secretContent "")
  get "/art" do
    html $ renderText (artContent pics)
  get "/video" do
    html $ renderText (videoContent vids)
  get "/woffer" do
    html $ renderText wofferContent
  post "/woffer" do
    fs <- files
    let fs' = fmap (\(_, fi) -> (fileContent fi, BSC8.unpack $ fileName fi)) fs
    liftIO $ mapM_ (\(a,b) -> BS.writeFile ("uploads" </> b) a) fs'
