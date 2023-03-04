{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class
import Control.Applicative
import Data.Bits
import Data.BitVector as BV
import Data.Char (ord)
import Data.Word
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Text as Text
import Lucid
import Math.NumberTheory.Primes
import Network.Socket qualified as Sk
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Web.Scotty

data Fortune = Fortune { id :: Int, fortune :: String } deriving Show

instance FromRow Fortune where
  fromRow = Fortune <$> field <*> field 

instance ToRow Fortune where
  toRow (Fortune id_ fortune) = toRow (id_, fortune)

main :: IO ()
main = do
  conn <- open "gematria.db"
  scottyApp (app conn) >>= run 23456 
  close conn

-- Conveniench.
io :: MonadIO m => IO a -> m a
io = liftIO

initGemDB :: Word64 -> Connection -> IO Fortune
initGemDB index conn = do
  execute_ conn 
    "CREATE TABLE IF NOT EXISTS tbl (id INTEGER PRIMARY KEY, fortune TEXT)"
--  execute conn
--    "INSERT INTO tbl (id, fortune) VALUES (?,?)" (Fortune 1 "Hello.")
  rowId <- maxId conn
  print rowId
  let rowId' = fromIntegral rowId
      magic = 1 + (index `mod` rowId') -- FUck!!! 1-indexed!!! Aughgrhg...
  (selected :: [Fortune]) <- 
    queryNamed 
      conn 
      "SELECT id,fortune FROM tbl WHERE id = :id" 
      [":id" := magic]
  return $ head selected

-- todo: make String into Text

app :: Connection -> ScottyM ()
app conn = do
  get "/" do 
    html $ renderText pageContentHome
  get "/gem" do
    req <- request
    let bigOmega :: Int
        bigOmega = fromIntegral . sum . (fmap snd) . factorise 
          $ hostAddressInt (remoteHost req)
        bigOmegaClamped = bigOmega `mod` length minorPlanets
        rz = ZipList $ fmap ord $ show req
        az = ZipList $ fmap ord 
          (mconcat . repeat $ minorPlanets !! bigOmegaClamped)
        gemScore :: Word64
        gemScore = fromIntegral . BV.nat . BV.join . (bitVecs 8) . getZipList 
          $ liftA2 (xor) rz az
        luckyNumber = bigOmegaClamped + 1 -- No one's lucky number should be 0!
    quote <- io $ initGemDB gemScore conn
    html $ renderText (pageContentFortune (fortune quote) luckyNumber)
  post "/gem" do
    (submittedFortune :: String) <- param "submittedFortune"
    io $ print submittedFortune
    io $ insertFortune conn submittedFortune
    redirect "/gem"

insertFortune :: Connection -> String -> IO ()
insertFortune conn fortune = do
  rowId <- maxId conn
  let rowId' = fromIntegral rowId
  print rowId'
  execute conn
    "INSERT INTO tbl (id, fortune) VALUES (?,?)"
    (Fortune (rowId' + 1) fortune)
  
maxId :: Connection -> IO Int
maxId conn = do
  [[res]] <- query_ conn "select MAX(id) from tbl;"
  pure res

minorPlanets :: [String]
minorPlanets =
  [ "CERES"
  , "PALLAS"
  , "JUNO"
  , "VESTA"
  , "ASTRAEA"
  , "HEBE"
  , "IRIS"
  , "FLORA"
  , "METIS"
  , "HYGIEA"
  , "PARTHENOPE"
  , "VICTORIA"
  , "EGERIA"
  , "IRENE"
  , "EUNOMIA"
  , "PSYCHE"
  ]


hostAddressInt :: Sk.SockAddr -> Int
hostAddressInt (Sk.SockAddrUnix _) = 0
hostAddressInt (Sk.SockAddrInet6 _ _ (a,b,c,d) _) = 
  sum $ fmap fromIntegral [a,b,c,d]
hostAddressInt (Sk.SockAddrInet _ ha) = fromIntegral ha

-- retrieve phrase from db on page load, not "when clicked"

pageContentFortune :: String -> Int -> Html ()
pageContentFortune fortune luckyNumber = do
  doctypehtml_ $ do
    style_ $ Text.pack $ unlines
      [ "@font-face {"
      , "  font-family: \"Manrope\";"
      , "  src: local(\"Manrope\"), url(\"Manrope-ExtraBold.woff2\"), format(\"woff\");"
      , "}"
      , "body {"
      , "  font-family: \"Manrope\";"
      , "  overflow: hidden;"
      , "  position: absolute;"
      , "  width: 100%;"
      , "  height: 100%;"
      , "  display: flex;"
      , "  justify-content: center;"
      , "  align-items: center;"
      , "  flex-direction: column;"
      , "  margin: 0;"
      , "}"
      , "h2 {"
      , "  font-size: 36px;"
      , "}"
      , "h3 {"
      , "  font-size: 24px;"
      , "}"
      ]
    body_ $ do
      h2_ $ toHtml fortune
      h3_ $ toHtml $ "Your lucky number is: " <> show luckyNumber
      form_ [method_ "post", action_ "/gem"] $ do
        input_ [type_ "text", name_ "submittedFortune"]
        input_ [type_ "submit"]

pageContentHome :: Html ()
pageContentHome = do
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
      , "  font-size: 40px;"
      , "  font-family: \"Manrope\";"
      , "  color: #fff;"
      , "  background-color: #000;"
      , "}"
      , "p {"
      , "  margin: 0px 0px 20px 0px;"
      , "}"
      , "h1 {"
      , "  margin: 0px 0px 20px 0px;"
      , "}"
      , ".container {"
      , "  margin: 20px auto;"
      , "  height: 100px;"
      , "  display: grid;"
      , "  grid-template-columns: 200px 200px;"
      , "  grid-row: auto auto;"
      , "  grid-column-gap: 20px;"
      , "  grid-row-gap: 20px;"
      , "}"
      , ".box {"
      , "  display: flex;"
      , "  justify-content: center;"
      , "  align-items: center;"
      , "}"
      ]
    head_ $ do
      title_ "help"
    body_ $ do 
      h1_ "You seem to be lost."
      div_ [class_ "container"] $ do
        div_ [class_ "box"] "1"
        div_ [class_ "box"] "2"
        div_ [class_ "box"] "3"
        div_ [class_ "box"] "4"
      p_ "Go away."
      a_ [href_ "twitter.com/nikshalark"] "@nikshalark"

-- note: for fortunes with the same ID, select by xor with DATE, not time
