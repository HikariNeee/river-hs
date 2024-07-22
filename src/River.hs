{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module River where

import Bits.Show
import Control.Monad (join)
import Data.Bits (shiftL)
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.List.Split
import System.Process

data View = Next | Previous
data Position = Left | Right | Up | Down
data Axis = Vertical | Horizontal
data Mode = Normal | Locked
data Modifiers = Super | Alt | Control | SA | SAC | SC | SS | None | AS | SAS

data RiverOpt
  = MkOpt !String
  | MkPar !String !String
  | MkPos !String !Position
  | MkAxis !String !Axis
  | MkView !String !View
  | MkTri !String !String !String
  | MkMode !String !Mode

instance Show RiverOpt where
  show (MkOpt a) = a
  show (MkPar a b) = (a ++ " " ++ b)
  show (MkPos a b) = (a ++ " " ++ (show b))
  show (MkAxis a b) = (a ++ " " ++ (show b))
  show (MkView a b) = (a ++ " " ++ (show b))
  show (MkTri a b c) = (a ++ " " ++ b ++ " " ++ c)
  show (MkMode a b) = (a ++ " " ++ (show b))

-- Nullary opts
rMap :: RiverOpt
rMap = MkOpt "map"

rToggleFloat :: RiverOpt
rToggleFloat = MkOpt "toggle-float"

rToggleFullscreen :: RiverOpt
rToggleFullscreen = MkOpt "toggle-fullscreen"

rZoom :: RiverOpt
rZoom = MkOpt "zoom"

rMapPointer :: RiverOpt
rMapPointer = MkOpt "map-pointer"

rResizeView :: RiverOpt
rResizeView = MkOpt "resize-view"

rMoveView :: RiverOpt
rMoveView = MkOpt "move-view"

rOnFocusChange :: RiverOpt
rOnFocusChange = MkOpt "on-focus-change"

rClose :: RiverOpt
rClose = MkOpt "close"

rExit :: RiverOpt
rExit = MkOpt "exit"

-- Unary opts
rSnap :: Position -> RiverOpt
rSnap a = MkPos "snap" a

rSetFocusedTags :: String -> RiverOpt
rSetFocusedTags a = MkPar "set-focused-tags" a

rSetViewTags :: String -> RiverOpt
rSetViewTags a = MkPar "set-view-tags" a

rToggleFocusedTags :: String -> RiverOpt
rToggleFocusedTags a = MkPar "toggle-focused-tags" a

rToggleViewTags :: String -> RiverOpt
rToggleViewTags a = MkPar "toggle-view-tags" a

rSpawn :: String -> RiverOpt
rSpawn a = MkPar "spawn" a

rSendLayoutCommand :: String -> RiverOpt
rSendLayoutCommand a = MkPar "send-layout-cmd" a

rSendToOutput :: View -> RiverOpt
rSendToOutput a = MkView "send-to-output" a

rFocusView :: View -> RiverOpt
rFocusView a = MkView "focus-view" a

rSwapView :: View -> RiverOpt
rSwapView a = MkView "swap-view" a

rFocusOutput :: View -> RiverOpt
rFocusOutput a = MkView "focus-output" a

rBackgroundColour :: String -> RiverOpt
rBackgroundColour a = MkPar "background-color" a

rBorderColourUnfocused :: String -> RiverOpt
rBorderColourUnfocused a = MkPar "border-color-unfocused" a

rBorderColourFocused :: String -> RiverOpt
rBorderColourFocused a = MkPar "border-color-focused" a

rRuleAdd :: String -> RiverOpt
rRuleAdd a = MkPar "rule-add" a

rDefaultLayout :: String -> RiverOpt
rDefaultLayout a = MkPar "default-layout" a

rDeclareMode :: String -> RiverOpt
rDeclareMode a = MkPar "declare-mode" a

rFocusFollowsCursor :: Mode -> RiverOpt
rFocusFollowsCursor a = MkMode "focus-follows-cursor" a

rSetCursorWarp :: String -> RiverOpt
rSetCursorWarp a = MkPar "set-cursor-warp" a

rSwap :: View -> RiverOpt
rSwap a = MkView "swap" a

-- binary opts
rResize :: Axis -> String -> RiverOpt
rResize a b = MkTri "resize" (show a) b

rMove :: Position -> String -> RiverOpt
rMove a b = MkTri "move" (show a) b

rSetRepeat :: String -> String -> RiverOpt
rSetRepeat a b = MkTri "set-repeat" a b

rHideCursor :: String -> String -> RiverOpt
rHideCursor a b = MkTri "hide-cursor" a b

data RiverCmdKb = RiverCmdKb
  { function :: RiverOpt
  , mode :: Mode
  , modifier :: Modifiers
  , key :: String
  , command :: RiverOpt
  }
  deriving (Show)

instance Show View where
  show Next = "next"
  show Previous = "previous"

instance Show Position where
  show River.Up = "up"
  show River.Down = "down"
  show River.Right = "right"
  show River.Left = "left"

instance Show Axis where
  show Vertical = "vertical"
  show Horizontal = "horizontal"

instance Show Modifiers where
  show Super = "Super"
  show Alt = "Alt"
  show Control = "Control"
  show SA = "Super+Alt"
  show SAC = "Super+Alt+Control"
  show SC = "Super+Control"
  show SS = "Super+Shift"
  show None = "None"
  show AS = "Alt+Shift"
  show SAS = "Super+Alt+Shift"

instance Show Mode where
  show Normal = "normal"
  show Locked = "locked"

callRiver :: [String] -> IO ()
callRiver a = callProcess "riverctl" a

splitAtQuote :: String -> [String]
splitAtQuote = (split . dropBlanks . dropDelims . whenElt) (== '\'')

riverToList :: RiverCmdKb -> [String]
riverToList a =
  if (join . splitAtQuote $ show $ command a) == (show $ command a)
    then
      concatMap words [show $ function a, show $ mode a, show $ modifier a, key a, show $ command a]
    else map (unwords . words) (join [[show $ function a], [show $ mode a], [show $ modifier a], [key a], z])
 where
  z = splitAtQuote $ show $ command a

riverMap :: Mode -> Modifiers -> String -> RiverOpt -> [String]
riverMap a b c d = riverToList $ RiverCmdKb{function = rMap, mode = a, modifier = b, key = c, command = d}

riverNormalMap :: Modifiers -> String -> RiverOpt -> [String]
riverNormalMap = riverMap Normal

riverMapPointer :: Mode -> Modifiers -> String -> RiverOpt -> [String]
riverMapPointer a b c d = bool x y (c `elem` ["BTN_LEFT", "BTN_RIGHT", "BTN_MIDDLE"])
 where
  y = riverToList $ RiverCmdKb{function = rMapPointer, mode = a, modifier = b, key = c, command = d}
  x = error "You are supposed to provide either BTN_LEFT,BTN_RIGHT or BTN_MIDDLE!"

riverNormalMapPointer :: Modifiers -> String -> RiverOpt -> [String]
riverNormalMapPointer = riverMapPointer Normal

riverBackgroundColour :: String -> [String]
riverBackgroundColour a = concatMap words [show $ rBackgroundColour a]

riverBorderColourFocused :: String -> [String]
riverBorderColourFocused a = concatMap words [show $ rBorderColourFocused a]

riverBorderColourUnfocused :: String -> [String]
riverBorderColourUnfocused a = concatMap words [show $ rBorderColourUnfocused a]

riverRuleAdd :: String -> [String]
riverRuleAdd a = concatMap words [show $ rRuleAdd a]

riverFocusFollowsCursor :: [String]
riverFocusFollowsCursor = concatMap words [show $ rFocusFollowsCursor Normal]

riverSetCursorWarp :: [String]
riverSetCursorWarp = concatMap words [show $ rOnFocusChange]

riverHideCursor :: String -> String -> [String]
riverHideCursor a b = concatMap words [show $ rHideCursor a b]

riverDefaultLayout :: String -> [String]
riverDefaultLayout a = concatMap words [show $ rDefaultLayout a]

riverSetRepeat :: String -> String -> [String]
riverSetRepeat a b = concatMap words [show $ rSetRepeat a b]

applyKeybinds :: [[String]] -> IO ()
applyKeybinds a = let x = a in for_ x callRiver

callExternal :: String -> [String] -> IO ()
callExternal a b = callProcess a b

computeTags :: Int -> Int
computeTags y = 1 `shiftL` (y - 1)

riverCreateTags :: Int -> IO ()
riverCreateTags y = for_ [1 .. y] h
 where
  h x =
    let
      tags = showFiniteBits $ computeTags x
     in
      applyKeybinds
        [ (riverMap Normal Super (show x) (rSetFocusedTags tags))
        , (riverMap Normal SS (show x) (rSetViewTags tags))
        , (riverMap Normal SC (show x) (rToggleFocusedTags tags))
        , (riverMap Normal SAC (show x) (rToggleViewTags tags))
        ]

riverAllTags :: String -> IO ()
riverAllTags x =
  let
    tag :: String
    tag = show $ ((1 :: Int) `shiftL` 32) - (1 :: Int)
   in
    applyKeybinds
      [ (riverMap Normal Super x (rSetFocusedTags $ tag))
      , (riverMap Normal SS x (rSetViewTags $ tag))
      ]
