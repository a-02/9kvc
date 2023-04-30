{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Content where

import Data.Text qualified as T
import Prelude hiding ((++))
import Lucid
import Style

homeContent :: Html ()
homeContent = do
  doctypehtml_ $ do
    style_ $ desktopAtRule $ T.concat [leftsideStyling ++ mainStyling]
    body_ $ div_ [class_ "menu"] leftSide

leftSide :: Html ()
leftSide = do
    a_ [href_ "/", class_ "insideLeft white"] $ h1_ "9k.vc"
    a_ [href_ "/tunes", class_ "insideLeft green "] $ h1_ "tunes"
    a_ [href_ "mailto:nks@9k.vc", class_ "insideLeft blue"] $  h1_ "e-mail"
    a_ [href_ "/text", class_ "insideLeft orange "] $ h1_ "text"
    a_ [href_ "/secret", class_ "insideLeft pink"] $ h1_ "secret"

tunesContent :: [(FilePath,String)] -> Html ()
tunesContent tunes = do
  doctypehtml_ $ do
    style_ . desktopAtRule $ T.concat 
      [ outsideStyling
      , leftsideStyling 
      , tunesStyling
      , mainStyling 
      ]
    body_ [class_ "outerflex"] $ do
      div_ [class_ "outerleft menu"] leftSide
      div_ [class_ "outerright musicbox"] $ 
        mapM_ (\(fp,txt) -> div_ [class_ "innermusicbox"] $
          do h3_ $ a_ [href_ ("/tunes/" ++ T.pack fp)] $ toHtml fp; 
             p_ $ toHtml txt;
        ) tunes
      
textContent :: [FilePath] -> Html ()
textContent texts = do
  doctypehtml_ $ do
    style_ . desktopAtRule $ T.concat 
      [ outsideStyling
      , leftsideStyling 
      , tunesStyling
      , mainStyling 
      ]
    body_ [class_ "outerflex"] $ do
      leftSide
      div_ [class_ "half"] $ 
        mapM_ (\fp -> 
          do p_ $ toHtml fp; 
             audio_ [controls_ "", src_ ("/texts/" ++ T.pack fp)] $ "";
        ) texts
    
secretContent :: Html ()
secretContent = undefined