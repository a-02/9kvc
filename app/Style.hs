{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Style where

import Data.Text qualified as T

homeStyling :: T.Text
homeStyling = T.unlines
  [ "body {"
  , "  background-color: #d9d9d9;"
  , "}"
  , ".outside {"
  , "  margin: 0;"
  , "}"
  , ".inside {"
  , "  display: block;"
  , "  height: 17vh;"
  , "}"
  , "img {"
  , "  height: 28vh;"
  , "  margin-left: -20px;"
  , "}"
  , "a:hover img {"
  , "  opacity: 0.5;"
  , "}"
  ]

mainStyling :: T.Text
mainStyling = T.unlines 
  [ "@font-face {"
  , "  font-family: \"Agave\";"
  , "  src: local(\"Agave\"), url(\"Agave-Regular.ttf\"), format(\"truetype\");"
  , "}"
  , "header {"
  , "  margin: 0;"
  , "  padding: 1px;"
  , "}"
  , "h1 {"
  , "  font-size: 40px;"
  , "  font-family: \"Agave\";"
  , "  margin: 10px;"
  , "}"
  , "h2 {"
  , "  border-bottom: 2px solid black;"
  , "  font-size: 30px;"
  , "  font-family: \"Agave\";"
  , "  margin: 20px 128px 10px 128px;"
  , "}"
  , "body {"
  , "  font-family: \"Agave\";"
  , "  background-color: #d9d9d9;"
  , "  color: #000;"
  , "  font-size: 30px;"
  , "}"
  , "p, ul {"
  , " margin: 0px 128px 0px 128px;"
  , " font-weight: normal;"
  , " font-size: 20px;"
  , "}"
  ]


