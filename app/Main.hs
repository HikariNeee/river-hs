{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Process
import Data.Foldable (for_)
import Data.Bool (bool)
import Data.Bits (shiftL)
import Bits.Show
import qualified Data.Vector as V

data View = Next | Previous
data Position = Left | Right | Up | Down
data Axis = Vertical | Horizontal
data Mode   = Normal | Locked 
data Modifiers = Super | Alt | Control | SA | SAC | SC |  SS | None

data RiverOpt = MkOpt !String
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
    show (MkAxis a b) = (a ++ " " ++  (show b))
    show (MkView a b) = (a ++ " " ++ (show b))
    show (MkTri a b c) = (a ++ " " ++ b ++ " " ++ c)
    show (MkMode a b) = (a ++ " " ++ (show b))

-- Nullary opts 
rMap = MkOpt "map"
rToggleFloat = MkOpt "toggle-float"
rToggleFullscreen = MkOpt "toggle-fullscreen"
rZoom = MkOpt "zoom"
rMapPointer = MkOpt "map-pointer"
rResizeView = MkOpt "resize-view"
rMoveView = MkOpt "move-view"
rOnFocusChange = MkOpt "on-focus-change"
rClose = MkOpt "close"

-- Unary opts
rSnap a = MkPos "snap" a
rSetFocusedTags a = MkPar "set-focused-tags" a
rSetViewTags a = MkPar "set-view-tags" a
rToggleFocusedTags a = MkPar "toggle-focused-tags" a
rToggleViewTags a = MkPar "toggle-view-tags" a
rSpawn a = MkPar "spawn" a
rSendLayoutCommand a = MkPar "send-layout-cmd" a
rSendToOutput a = MkView "send-to-output" a
rFocusView a = MkView "focus-view" a
rSwapView a = MkView "swap-view" a
rFocusOutput a = MkView "focus-output" a
rBackgroundColour a = MkPar "background-color" a
rBorderColourUnfocused a = MkPar "border-color-unfocused" a
rBorderColourFocused a = MkPar "border-color-focused" a
rRuleAdd a = MkPar "rule-add" a
rDefaultLayout a = MkPar "default-layout" a
rDeclareMode a = MkPar "declare-mode" a
rFocusFollowsCursor a = MkMode "focus-follows-cursor" a
rSetCursorWarp a = MkPar "set-cursor-warp" a
rSwap a = MkView "swap" a

-- binary opts
rResize a b = MkTri "resize" a  b
rMove a b = MkTri "move" a b
rSetRepeat a b = MkTri "set-repeat" a b
rHideCursor a b = MkTri "hide-cursor" a b

data RiverCmdKb = RiverCmdKb {
   function :: RiverOpt, 
   mode :: Mode,
   modifier :: Modifiers,
   key :: String,
   command :: RiverOpt
  } deriving (Show)

instance Show View where
    show Next = "next"
    show Previous = "previous"

instance Show Position where
    show Main.Up = "up"
    show Main.Down = "down"
    show Main.Right = "right"
    show Main.Left = "left"

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


instance Show Mode where
    show Normal = "normal"
    show Locked = "locked"


callRiver :: [String] -> IO ()
callRiver a = callProcess "riverctl" a


riverToList :: RiverCmdKb -> [String]
riverToList a = concatMap words [show $ function a, show $ mode a,show $ modifier a,key a, show $ command a]


riverMap :: Mode -> Modifiers -> String -> RiverOpt -> [String]
riverMap a b c d =  riverToList $ RiverCmdKb {function = rMap, mode = a, modifier = b, key = c, command = d}

riverMapPointer :: Mode -> Modifiers -> String -> RiverOpt -> [String]
riverMapPointer a b c d = bool x y (c `elem` ["BTN_LEFT", "BTN_RIGHT", "BTN_MIDDLE"]) 
                          where y = riverToList $  RiverCmdKb {function = rMapPointer, mode = a, modifier = b, key = c, command = d}
                                x = error "You are supposed to provide either BTN_LEFT,BTN_RIGHT or BTN_MIDDLE!"


riverBackgroundColour :: String -> [String]
riverBackgroundColour a = concatMap words [show $ rBackgroundColour a]

riverBorderColourFocused :: String -> [String]
riverBorderColourFocused a = concatMap words [show $ rBorderColourFocused a]


riverBorderColourUnfocused :: String -> [String]
riverBorderColourUnfocused a = concatMap words [show $ rBorderColourUnfocused a]


riverRuleAdd :: String -> [String]
riverRuleAdd a = concatMap words [show $ rRuleAdd a]


riverDefaultLayout :: String -> [String]
riverDefaultLayout a = concatMap words [show $ rDefaultLayout a]

--applyKeybinds :: [[String]] -> IO ()
applyKeybinds a = let x = a in for_ x callRiver

callExternal :: String -> [String] -> IO ()
callExternal a b = callProcess a b

computeTags ::  Int ->  Int
computeTags y = 1 `shiftL` (y - 1)

riverCreateTags x = for_ [1..x] h
  where h x = let
                 tags = showFiniteBits $ computeTags x 
              in 
                 applyKeybinds [
                             (riverMap Normal Super (show x) (rSetFocusedTags tags)),
                             (riverMap Normal SS (show x) (rSetViewTags tags)),
                             (riverMap Normal SC (show x) (rToggleFocusedTags tags)),
                             (riverMap Normal SAC (show x) (rToggleViewTags tags))]
                               
riverAllTags key = let
                 tag :: String
                 tag = show $ ((1 :: Int) `shiftL` 32) - (1 :: Int)
               in
                 applyKeybinds [
                             (riverMap Normal Super key (rSetFocusedTags $ tag)),
                             (riverMap Normal SS key (rSetViewTags $ tag))]


main = riverCreateTags 5 >> riverAllTags "0" >> applyKeybinds [(riverMap Normal Super "x" (rSpawn "beep"))]
