module Main where

import Data.Array
import Data.Either
import Prelude
import Sketch.Types
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Lib.Layers (showGroupLayer, showImageLayer, showShapeLayer, showTextLayer)
import Lib.Sketch (copy2Pasteboard)
import Sketch.Dom as Dom
import Sketch.UI as UI


myCommand :: Effect Unit
myCommand = do
    Dom.selectedLayers >>= case _ of
        Left err -> UI.message "Something went wrong..."
        Right layers -> do
            if length layers == 0 
                then UI.alert "No Selection" "Please select a layer and try again..."
                else do 
                    UI.alert "Success ✅" "Copied view to clipboard"
                    copy2Pasteboard $ createViewFromLayer (head layers)


createViewFromLayer :: Maybe Layer -> String 
createViewFromLayer (Just (Text tl)) = showTextLayer tl
createViewFromLayer (Just (Image tl)) = showImageLayer tl
createViewFromLayer (Just (Shape tl)) = showShapeLayer tl
createViewFromLayer (Just (Group tl)) = showGroupLayer tl
createViewFromLayer _ = "unit" 

