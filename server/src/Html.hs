{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

-- *** THIS MODULE IS A WORK IN PROGRESS ***
-- TODO:
--  - [x] render tree to json and include in template
--  - [ ] incorporate application code from javascript modules
--  - [ ] write tests

module Html (renderHtml) where

import Data.ByteString.Lazy.UTF8 (ByteString, toString)
import Data.Aeson (encode)
import Text.Hamlet (shamlet, Html)
import Data.Text (Text)
import Text.Blaze.Html.Renderer.String (renderHtml)
import TaskTree

template :: TaskTreeNode -> Html
template tree = let treejson = toString . encode $ tree
                    appscript = "" :: String
                   in [shamlet|
$doctype 5
<html>
  <head>
    <title>Grasana: Graphical Asana Projects
    <style>svg.panning text { user-select: none; }
    <script language="javascript">window.treejson = #{treejson}
  <body>
    <div id="AppContainer">
    <script language="javascript">#{appscript}
|]
