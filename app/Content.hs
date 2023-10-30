{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Content where

import Data.Text qualified as T
import Prelude hiding ((++))
import Lucid
import Style

homeContent :: Int -> Html ()
homeContent i = do
  doctypehtml_ $ do
    style_ $ desktopAtRule $ T.concat [mainStyling ++ leftsideStyling]
    body_ $ div_ [class_ "menu"] (leftSide i)

leftSide :: Int -> Html ()
leftSide i = sequence_ . take 5 . drop i . cycle $
    [ a_ [href_ "/", class_ "insideLeft white"] $ h1_ "9k.vc"
    , a_ [href_ "/tunes", class_ "insideLeft green "] $ h1_ "NIGHTMARE WORLD"
    , a_ [href_ "mailto:nks@9k.vc", class_ "insideLeft blue"] $  h1_ "ELECTRONIC MAIL"
    , a_ [href_ "/text", class_ "insideLeft orange "] $ h1_ "MASSIVE BLOGPOSTS"
    , a_ [href_ "/art", class_ "insideLeft red "] $ h1_ "DOPE PICS"
    , a_ [href_ "/video", class_ "insideLeft pink"] $ h1_ "SICK VIDZ"
    ]

tunesContent :: [(FilePath,String)] -> Html ()
tunesContent tunes = do
  doctypehtml_ $ do
    style_ . desktopAtRule $ T.concat
      [ outsideStyling
      , leftsideStyling
      , mainStyling
      , tunesStyling
      ]
    body_ [class_ "outerflex"] $ do
      div_ [class_ "outerleft menu"] (leftSide 2)
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
      , mainStyling
      , textsStyling
      ]
    body_ [class_ "outerflex"] $ do
      div_ [class_ "outerleft menu"] (leftSide 4)
      div_ [class_ "outerright textbox"] $
        mapM_ (\fp -> div_ [class_ "innertextbox"] $
          do h3_ $ a_ [href_ ("/texts/" ++ T.pack fp)] $ toHtml fp
        ) texts

artContent :: [FilePath] -> Html ()
artContent pics = do
  doctypehtml_ $ do
    style_ . desktopAtRule $ T.concat
      [ outsideStyling
      , leftsideStyling
      , mainStyling
      , secretStyling
      ]
    body_ [class_ "outerflex"] $ do
      div_ [class_ "outerleft menu"] (leftSide 5)
      div_ [class_ "outerright textbox"] $
        mapM_ (\fp -> 
          do img_ [src_ ("/art/" ++ T.pack fp)]
        ) pics

videoContent :: [FilePath] -> Html ()
videoContent vids = do
  doctypehtml_ $ do
    style_ . desktopAtRule $ T.concat
      [ outsideStyling
      , leftsideStyling
      , mainStyling
      , textsStyling
      ]
    body_ [class_ "outerflex"] $ do
      div_ [class_ "outerleft menu"] (leftSide 3)
      div_ [class_ "outerright textbox"] $
        mapM_ (\fp -> video_ [controls_ "", width_ "30%"] $ source_ [src_ ("/video/" ++ T.pack fp), type_ "video/mp4"]
        ) vids

secretContent :: String -> Html ()
secretContent myScript =
  doctypehtml_ $ do
    script_ $ T.pack myScript
    style_ . desktopAtRule $ T.concat
      [ outsideStyling
      , leftsideStyling
      , mainStyling
      ]
    body_ [class_ "outerflex"] $ do
      div_ [class_ "outerleft menu"] (leftSide 1)
      div_ [class_ "outterright textbox"] $
        h1_ [id_ "secret"] "???"



wofferContent :: Html ()
wofferContent = doctypehtml_ $ do
  body_ $ do
    h1_ "the woffer..."
    form_ [action_ "/woffer", enctype_ "multipart/form-data", method_ "post"] $ do
      input_ [id_ "file", type_ "file", name_ "file"]
      label_ [id_ "file-label", class_ "btn-1", for_ "file"] "upload that shit son"
      button_ [id_ "submit", type_ "submit"] "go! go!!!!"
