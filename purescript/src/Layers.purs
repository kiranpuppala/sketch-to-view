module Lib.Layers where

import Data.Array ((!!))
import Debug.Trace (spy)
import Effect (Effect)
import Lib.Parser (setCornerRadius, setFrame, setGroupStyle, setHidden, setImageStyle, setImageUrl, setShapeDrawable, setShapeStyle, setText, setTextStyle)
import Prelude (Unit, (<>))
import Sketch.Types (GroupLayer(..), ImageLayer(..), ShapeLayer(..), TextLayer(..))


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
showShapeLayer sl@(ShapeLayer s) = do 
  case s.shapeType of 
    "Rectangle" -> "<LinearLayout \n" <> 
                    setFrame s.frame <>
                    setHidden s.hidden <>
                    setShapeStyle s.style <>
                    " />"

    "Oval" -> "<LinearLayout \n" <> 
                setFrame s.frame <>
                setHidden s.hidden <>
                " />\n"<> 
                (setShapeDrawable "oval" s.style)
    _ -> "<LinearLayout \n" <> 
          setFrame s.frame <>
          setShapeStyle s.style <>
          " />"

         