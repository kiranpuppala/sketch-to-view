module Lib.Parser where
  
import Prelude (map, show, ($), (*), (-), (/), (<>)) 

import Data.Array (filter, (!!)) 
import Data.Int (floor)
import Data.Maybe (Maybe(..)) 
import Data.String (joinWith, toLower, toUpper) 
import Data.String.CodePoints (splitAt) 
import Math (atan2, pi) 

import Sketch.Types (Border(..), Fill(..), Frame(..), Gradient(..), GroupStyle(..), ImageStyle(..), Point(..), Points(..), Shadow(..), ShapeStyle(..), Stop(..), TextStyle(..)) 
import Lib.Utils (Radix(..), makeColorStr, num2Str, parseNumber) 


-- | PARAMS -------------------------------------------------------------------

setText :: String -> String
setText text = "android:text = " <> show text <> "\n"

setHidden :: Boolean -> String
setHidden false = ""
setHidden true  = "android:visibility = \"gone" <> "\"\n"

setOpacity :: Number -> String
setOpacity 1.0 = ""
setOpacity opacity = "android:alpha = \"" <> num2Str opacity <> "\"\n"

setKerning :: Maybe Number -> String
setKerning Nothing    = ""
setKerning (Just 0.0) = ""
setKerning (Just k)   = "android:letterSpacing = \"" <> num2Str k <> "dp\"\n"

setLineHeight :: Maybe Number -> String
setLineHeight Nothing     = ""    
setLineHeight (Just 0.0)  = ""
setLineHeight (Just l)    = "android:lineHeight = \"" <> show (floor l) <> "dp\"\n"

setGravity :: String -> String
setGravity "left"   = ""
setGravity "right"  = "android:gravity = right" <> "\"\n"
setGravity "center" = "android:gravity = center" <> "\"\n"
setGravity _        = "android:gravity = left" <> "\"\n"

setCornerRadius :: Maybe Points -> String
setCornerRadius Nothing           = ""
setCornerRadius (Just (Points p)) = "  , cornerRadius " <>  show p.cornerRadius <> "\"\n"

setColor :: String -> String
setColor color = "android:textColor = " <> show (makeColorStr color) <> "\n"

setFontSize :: Number -> String
setFontSize fontSize = "android:textSize= \"" <> show (floor fontSize) <> "dp\"\n"

setFrame :: Frame -> String
setFrame (Frame f) = 
  "android:layout_height=\"" <> show (floor f.height) <> "dp\"\n" <>
  "android:layout_width=\"" <> show (floor f.width) <> "dp\"\n"

setImageUrl :: String -> String
setImageUrl url = "android:src = \"@drawable/" <> (toLower url) <> "\"\n"

setBackground :: Maybe (Array Fill) -> String
setBackground Nothing = ""
setBackground (Just fills) = do
  let enabledFills = filter (\(Fill a) -> a.enabled) fills 
  case enabledFills !! 0 of
    Nothing -> ""
    Just (Fill a) -> do
      case a.fill of
        "Color" -> "android:background = " <> show (makeColorStr a.color) <> "\n"
        "Gradient" -> let (Gradient g) = a.gradient
                          gradientType = "Linear"
                          colors = joinWith ", " $ map (\(Stop stop) -> show $ makeColorStr stop.color) g.stops
                          (Point from) = g.from
                          (Point to) = g.to
                          degree = num2Str $ atan2 (to.x - from.x) (to.y - from.y) * 180.0 / pi
                      in "  , gradient $ " <> gradientType <> " " <> degree <> " [" <> colors <> "]\"\n" 
        _ -> " android:background = "<> show (makeColorStr a.color) <> "\n"

setBorder :: Maybe (Array Border) -> String
setBorder Nothing = ""
setBorder (Just borders) = do
  let enabledBorders = filter (\(Border a) -> a.enabled) borders 
  case enabledBorders !! 0 of
    Nothing -> ""
    Just (Border a) -> ""

setShadow :: Maybe (Array Shadow) -> String
setShadow Nothing = ""
setShadow (Just shadows) = do
  let enabledShadows = filter (\(Shadow a) -> a.enabled) shadows 
  case enabledShadows !! 0 of
    Nothing -> ""
    Just (Shadow a) -> let color1 = (splitAt 1 a.color).after
                           color2 = (splitAt 6 color1).before
                           opacityHex = (splitAt 6 color1).after 
                           color = show $ toUpper ("#" <> color2)
                           opacity = num2Str ((parseNumber opacityHex Hex) * 100.0 / 25500.0)
                           values = joinWith " " $ map (\num -> num2Str num) [ a.x, a.y, a.blur, a.spread ]
                        in "  , shadow $ Shadow " <> values  <> " " <> color <> " " <> opacity <> "\"\n"

-- | FONTS --------------------------------------------------------------------

setFontFamily :: String -> Number -> String
setFontFamily fontFamily fontWeight =
    "android:fontFamily = \"" <> fontFamily <> "-" <>
    case fontWeight of
      2.0 -> "UltraLight"
      3.0 -> "Light"
      5.0 -> "Regular"
      6.0 -> "Medium"
      8.0 -> "SemiBold"
      9.0 -> "Bold"
      10.0 -> "Heavy"
      11.0 -> "Black"
      _ -> "Regular"
    <> "\"\n"

-- | STYLES -------------------------------------------------------------------

setTextStyle :: TextStyle -> String
setTextStyle (TextStyle t) = 
  setGravity t.alignment <>
  setOpacity t.opacity <>
  setKerning t.kerning <>
  setLineHeight t.lineHeight <>
  setColor t.textColor <>
  setFontSize t.fontSize <>
  setFontFamily t.fontFamily t.fontWeight

setGroupStyle :: GroupStyle -> String
setGroupStyle (GroupStyle g) = 
  setOpacity g.opacity

setImageStyle :: ImageStyle -> String
setImageStyle (ImageStyle i) =
  setOpacity i.opacity

setShapeStyle :: ShapeStyle -> String
setShapeStyle (ShapeStyle s) =
  setOpacity s.opacity <>
  setBackground s.fills 

setShapeDrawable :: String -> ShapeStyle -> String 
setShapeDrawable shapeType (ShapeStyle s) = do
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" <>
  " <shape\n" <>
  "     xmlns:android=\"http://schemas.android.com/apk/res/android\"\n" <>
  "     android:shape=\""<> shapeType <>"\">\n" <>
  "     "<> (setShapeFills s.fills) <>
  "     "<> (setShapeBorders s.borders)<>
  "</shape>\n"

setShapeFills :: Maybe (Array Fill) -> String 
setShapeFills Nothing = ""
setShapeFills (Just fills) = case fills !! 0 of 
    Nothing -> ""
    Just (Fill a) -> case a.fill of
      "Color" -> "<solid android:color=" <> show (makeColorStr a.color) <> "/>\n"
      "Gradient" -> ""
      _ -> ""

setShapeBorders :: Maybe (Array Border) -> String 
setShapeBorders Nothing = ""
setShapeBorders (Just borders) = case borders !! 0 of 
  Nothing -> ""
  Just (Border a) -> "<stroke android:color=" <> show (makeColorStr a.color) <> "android:width=\""<> (show a.thickness) <> "dp\"/>\n"

