{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Content where

import Lucid
import Style

homeContent :: Html ()
homeContent = do
  doctypehtml_ $ do
    style_ homeStyling
    body_ $ do
      div_ [class_ "outside"] $ do
        a_ [href_ "/", class_ "inside"] $ img_ [src_ "thing1f.png"]
        a_ [href_ "/about", class_ "inside"] $ img_ [src_ "thing2f.png"]
        a_ [href_ "/email", class_ "inside"] $ img_ [src_ "thing3f.png"]
        a_ [href_ "/text", class_ "inside"] $ img_ [src_ "thing4f.png"]
        a_ [href_ "/secret", class_ "inside"] $ img_ [src_ "thing5f.png"]

aboutContent :: Html ()
aboutContent = undefined
emailContent :: Html ()
emailContent = undefined
textContent :: Html ()
textContent = undefined
secretContent :: Html ()
secretContent = undefined
