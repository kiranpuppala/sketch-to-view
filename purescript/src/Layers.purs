module Lib.Layers where

import Prelude ((<>)) 

import Data.Array ((!!)) 

import Sketch.Types (GroupLayer(..), ImageLayer(..), ShapeLayer(..), TextLayer(..)) 
import Lib.Parser (setCornerRadius, setFrame, setGroupStyle, setHidden, setImageStyle, setImageUrl, setShapeStyle, setText, setTextStyle) 

showTextLayer :: TextLayer -> String
showTextLayer (TextLayer t) = 
  "<TextView \n " <>
  setFrame t.frame <>
  setHidden t.hidden <>
  setText t.text <>
  setTextStyle t.style <>
  "  />"

showGroupLayer :: GroupLayer -> String
showGroupLayer (GroupLayer g) = 
  "<LinearLayout \n" <> 
  setFrame g.frame <>
  setHidden g.hidden <>
  setGroupStyle g.style <>
  " />"

showImageLayer :: ImageLayer -> String
showImageLayer (ImageLayer i) = 
  "<ImageView \n" <> 
  setFrame i.frame <>
  setImageUrl i.name <>
  setHidden i.hidden <>
  setImageStyle i.style <>
  "  />"

showShapeLayer :: ShapeLayer -> String
showShapeLayer (ShapeLayer s) = 
  case s.shapeType of 
    "Rectangle" -> "<LinearLayout \n" <> 
                    setFrame s.frame <>
                    setHidden s.hidden <>
                    setShapeStyle s.style <>
                    " />"
    _ -> "<LinearLayout \n" <> 
          setFrame s.frame <>
          " />"
