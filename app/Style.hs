{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Style where

import Prelude hiding ((++))
import Data.Text qualified as T

(++) :: T.Text -> T.Text -> T.Text
(++) = T.append

desktopAtRule :: T.Text -> T.Text
desktopAtRule inner =
  "@media (min-width: 768px) { " ++ inner ++ " }"

mobileAtRule :: T.Text -> T.Text
mobileAtRule inner =
  "@media (max-width: 767px) { " ++ inner ++ " }"

outsideStyling :: T.Text
outsideStyling = T.unlines
  [ ".outerflex {"
  , "  display: flex;"
  , "  flex-direction: row;"
  , "}"
  , ".outerleft {"
  , "  width: 33%;"
  , "  box-sizing: border-box;"
  , "}"
  , ".outerright {"
  , "  width: 67%;"
  , "  box-sizing: border-box;"
  , "}"
  ]

tunesStyling :: T.Text
tunesStyling = T.unlines
  [ "p {"
  , "  font-size: 0.8em;"
  , "  margin: 0;"
  , "}"
  , ".musicbox {"
  , "  display: flex;"
  , "  margin: 10px;"
  , "  flex-direction: row;"
  , "  flex-wrap: wrap;"
  , "  gap: 10px;"
  , "  color: #afd900;"
  , "  height: 10%;"
  , "}"
  , ".innermusicbox {"
  , "  box-shadow: 0px 1px 3px rgba(0,0,0,0.4);"
  , "  border: solid 2px #fff;"
  , "  padding: 10px;"
  , "}" 
  , "h3 {" 
  , "  font-size: 1em;"
  , "  margin: 0;"
  , "}"
  , "a {"
  , "  color: #afd900;"
  , "}"
  ]

textsStyling :: T.Text
textsStyling = T.unlines
  [ "p {"
  , "  font-size: 0.8em;"
  , "  margin: 0;"
  , "}"
  , ".textbox {"
  , "  display: flex;"
  , "  margin: 10px;"
  , "  flex-direction: row;"
  , "  flex-wrap: wrap;"
  , "  gap: 10px;"
  , "  color: #e09e00;"
  , "  height: 10%;"
  , "}"
  , ".innertextbox {"
  , "  box-shadow: 0px 1px 3px rgba(0,0,0,0.4);"
  , "  border: solid 2px #fff;"
  , "  padding: 10px;"
  , "}" 
  , "h3 {" 
  , "  font-size: 2em;"
  , "  margin: 0;"
  , "}"
  , "a {"
  , "  color: #e09300;"
  , "}"
  ]



mainStyling :: T.Text
mainStyling = T.unlines 
  [ "@font-face {"
  , "  font-family: \"Agave\";"
  , "  src: url(\"/font/Agave-Regular.woff2\"), format(\"woff\");"
  , "}"
  , "body {"
  , "  font-family: \"Agave\";"
  , "  background-color: #000;"
  , "}"
  ]

{-

green: afd900
blue: 4fcde0
orange: e09300
pink: e16aa1

-}
leftsideStyling :: T.Text
leftsideStyling = T.unlines
  [ ".menu {"
  , "  margin: 0;"
  , "  display: grid;"
  , "}"
  , ".insideLeft {"
  , "  display: block;"
  , "  text-decoration: none;"
  , "}"
  , "h1 {"
  , "  margin: 3vh 0px 0px 0px;"
  , "  font-size: 9vh;"
  , "}"
  , ".white {"
  , "  color: white;"
  , "  border-bottom: solid 10px white;"
  , "}"
  , ".green {"
  , "  color: #afd900;"
  , "  border-bottom: solid 10px #afd900;"
  , "}"
  , ".blue {"
  , "  color: #4fcde0;"
  , "  border-bottom: solid 10px #4fcde0;"
  , "}"
  , ".orange {"
  , "  color: #e09300;"
  , "  border-bottom: solid 10px #e09300;"
  , "}"
  , ".pink {"
  , "  color: #e16aa1;"
  , "  border-bottom: solid 10px #e16aa1;"
  , "}"
  , "a:hover  {"
  , "  opacity: 0.5;"
  , "}"
  ]

secretStyling :: T.Text
secretStyling = T.unlines
  [ "p {"
  , "  font-size: 0.8em;"
  , "  margin: 0;"
  , "}"
  , ".textbox {"
  , "  display: flex;"
  , "  margin: 10px;"
  , "  flex-direction: row;"
  , "  flex-wrap: wrap;"
  , "  gap: 10px;"
  , "  color: #e16aa1;"
  , "  height: 10%;"
  , "}"
  , ".innertextbox {"
  , "  box-shadow: 0px 1px 3px rgba(0,0,0,0.4);"
  , "  border: solid 2px #fff;"
  , "  padding: 10px;"
  , "}" 
  , "h3 {" 
  , "  font-size: 2em;"
  , "  margin: 0;"
  , "}"
  , "a {"
  , "  color: #e16aa1;"
  , "}"
  ]


