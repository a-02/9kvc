{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class
import Control.Applicative
import Data.Bits
import Data.BitVector as BV hiding (not)
import Data.Char (ord)
import Data.Maybe
import Data.Word
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Text as Text
import Lucid
import Math.NumberTheory.Primes
import Network.Socket qualified as Sk
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static (static)
import Web.Scotty

data Fortune = Fortune { id_ :: Int, fortune :: String, time :: Int, address :: String } deriving Show

instance FromRow Fortune where
  fromRow = Fortune <$> field <*> field <*> field <*> field 

instance ToRow Fortune where
  toRow (Fortune id_ fortune time address) = toRow (id_, fortune, time, address)

main :: IO ()
main = do
  conn <- open "gematria.db"
  scottyApp (app conn) >>= run 2345
  close conn

--  execute conn
--    "INSERT INTO tbl (id, fortune, time, address) VALUES (?,?,?,?)" (Fortune 1 "Hello." 1 "what")

-- Conveniench.
io :: MonadIO m => IO a -> m a
io = liftIO

initGemDB :: Word64 -> Connection -> IO Fortune
initGemDB index conn = do
  execute_ conn 
    "CREATE TABLE IF NOT EXISTS tbl (id INTEGER PRIMARY KEY, fortune TEXT, time INTEGER, address TEXT)"
  rowId <- maxId conn
  print rowId
  let rowId' = fromIntegral rowId
      magic = 1 + (index `mod` rowId') -- FUck!!! 1-indexed!!! Aughgrhg...
  print "grabbing fortune..."
  (selected :: [Fortune]) <- 
    queryNamed 
      conn 
      "SELECT id,fortune,time,address FROM tbl WHERE id = :id" -- if you fuck up the db, fuck up this line too.
      [":id" := magic]
  print "successfully grabbed fortune"
  return $ head selected

-- todo: make String into Text

app :: Connection -> ScottyM ()
app conn = do
  middleware static
  get "/" do 
    html $ renderText pageContentHome
  get "/about" do
    html $ renderText pageContentAbout
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
        ha = hostAddressShow $ remoteHost req
    io $ print "starting fortune grab"
    quote <- io $ initGemDB gemScore conn
    time <- io $ getPOSIXTime
    io $ print "testing for recency"
    submittedRecently <- io $ hasSubmittedRecently conn time ha
    html $ renderText (pageContentFortune (fortune quote) luckyNumber submittedRecently)
  post "/gem" do
    req <- request
    time <- io $ getPOSIXTime 
    let ha = hostAddressShow (remoteHost req)
    (submittedFortune :: String) <- param "submittedFortune"
    io $ print submittedFortune
    io $ insertFortune conn submittedFortune time ha
    redirect "/gem"

hasSubmittedRecently :: Connection -> POSIXTime -> String -> IO Bool
hasSubmittedRecently conn time ha = do 
  print "grabbing most recent from address"
  (outer@[[res]] :: [[Maybe Int]]) <- queryNamed conn "select MAX(time) from tbl where address = :address" [":address" := ha] -- what?
  let timeInt = floor . nominalDiffTimeToSeconds $ time
      res' = maybe 0 id res
  print "success recency"
  print timeInt
  print res'
  return . not $ (null outer) || (isNothing res) || ((abs $ timeInt - res') <= (fromEnum $ nominalDiffTimeToSeconds nominalDay)) -- this is ugly?

-- TODO: Add a logger.

insertFortune :: Connection -> String -> POSIXTime -> String -> IO ()
insertFortune conn fortune time address = do
--  print "starting fortune insert"
  rowId <- maxId conn
  let rowId' = fromIntegral rowId
      timeInt = floor . nominalDiffTimeToSeconds $ time
--  print rowId'
  execute conn
    "INSERT INTO tbl (id, fortune, time, address) VALUES (?,?,?,?)"
    (Fortune (rowId' + 1) fortune timeInt address)
  
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

hostAddressShow :: Sk.SockAddr -> String
hostAddressShow (Sk.SockAddrUnix unix) = unix
hostAddressShow (Sk.SockAddrInet _ ha) = show ha
hostAddressShow (Sk.SockAddrInet6 _ _ ha6 _) = show ha6

-- retrieve phrase from db on page load, not "when clicked"

pageContentAbout :: Html ()
pageContentAbout = do
  doctypehtml_ $ do
    style_ $ Text.pack $ unlines
      [ "@font-face {"
      , "  font-family: \"Manrope\";"
      , "  src: local(\"Manrope\"), url(\"Manrope-ExtraBold.woff2\"), format(\"woff\");"
      , "}"
      , "body {"
      , "  font-family: \"Manrope\";"
      , "  background-color: #ccc;"
      , "  color: #000;"
      , "}"
      ]
    body_ $ do
      p_ "Plot of internet land owned by nikshalark. You seem to be lost."
      p_ "My interests include functional programmimng & computer music."

pageContentFortune :: String -> Int -> Bool -> Html ()
pageContentFortune fortune luckyNumber submittedRecently = do
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
      , "  color: #d46;"
      , "  background-color: #edc;"
      , "  margin: 0;"
      , "}"
      , "h2 {"
      , "  font-size: 36px;"
      , "  margin: 60px 0px;"
      , "}"
      , "h3 {"
      , "  font-size: 24px;"
      , "  margin: 0;"
      , "}"
      ]
    body_ $ do
      h3_ "Your fortune is:"
      h2_ $ toHtml fortune
      h3_ $ toHtml $ "Your lucky number is: " <> show luckyNumber
      if submittedRecently 
      then form_ [method_ "post", action_ "/gem"] $ do
             input_ [type_ "text", name_ "submittedFortune"]
             input_ [type_ "submit"]
      else h3_ "Wait a bit before submitting more."

pageContentHome :: Html ()
pageContentHome = do
  doctypehtml_ $ do 
    style_ $ Text.pack $ unlines
      [ "@font-face {"
      , "  font-family: \"Nacelle\";"
      , "  src: url(\"Nacelle-BlackItalic.otf\"), format(\"opentype\");"
      , "}"
      , "@font-face {"
      , "  font-family: \"Manrope\";"
      , "  src: url(\"Manrope-ExtraBold.woff2\"), format(\"woff\");"
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
      , "  color: #fff;"
      , "  background-color: #c40;"
      , "  font-family: \"Manrope\";"
      , "  font-size: 24px;"
      , "}"
      , "p {"
      , "  margin: 0px 0px 20px 0px;"
      , "}"
      , "h1 {"
      , "  margin: 0px 0px 0px 0px;"
      , "  font-family: \"Nacelle\";"
      , "  font-size: 60px;"
      , "}"
      , "a {"
      , "  outline: none;" 
      , "  text-decoration: none;"
      , "}"
      , "a:link {"
      , "  color: #c40;" 
      , "  text-decoration: none;"
      , "}"
      , "a:visited {"
      , "  color: #c40;" 
      , "  text-decoration: none;"
      , "}"
      , ".container {"
      , "  display: grid;"
      , "  grid-template-columns: auto auto;"
      , "  grid-column-gap: 20px;"
      , "  grid-row-gap: 20px;"
      , "}"
      , ".box {"
      , "  display: flex;"
      , "  justify-content: center;"
      , "  align-items: center;"
      , "  background-color: #eee;"
      , "  color: #000;"
      , "  padding: 10px;"
      , "  border-radius: 5px;"
      , "}"
      ]
    head_ $ do
      title_ "help"
    body_ $ do 
      h1_ "You seem to be lost."
      div_ [class_ "container"] $ do
        div_ [class_ "box"] $ do
          a_ [href_ "/gem"] "Fortune."
        div_ [class_ "box"] "Out of order."
        div_ [class_ "box"] "Out of order."
        div_ [class_ "box"] $ do
          a_ [href_ "/about"] "About."
