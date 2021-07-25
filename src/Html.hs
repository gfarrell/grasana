{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}

module Html (makeHtml) where

import Data.ByteString.Lazy.UTF8 (ByteString, toString)
import Data.Aeson (encode)
import Text.Hamlet (shamlet, Html)
import Data.Text (Text)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (preEscapedToMarkup)
import TaskTree

import Language.Haskell.TH
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString
import Data.FileEmbed (embedFile, makeRelativeToProject)

jsAppData :: Data.ByteString.ByteString
jsAppData = $(makeRelativeToProject "js/dist/main.js" >>= embedFile)

template :: TaskTreeNode -> Html
template tree = let treejson = toString . encode $ tree
                    appjs    = TE.decodeUtf8 jsAppData
                   in [shamlet|
$doctype 5
<html>
  <head>
    <title>Grasana: Graphical Asana Projects
    <style>svg.panning text { user-select: none; }
    <script language="javascript">window.treejson = #{preEscapedToMarkup treejson}
  <body>
    <div id="AppContainer">
    <script language="javascript">#{preEscapedToMarkup appjs}
|]

makeHtml :: TaskTreeNode -> String
makeHtml = renderHtml . template
