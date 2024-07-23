module Main where

import Data.Foldable (for_)
import qualified River as R

initialKeymap :: [[String]]
initialKeymap =
  [ (R.riverNormalMap R.Alt "Return" $ R.rSpawn "foot")
  , (R.riverNormalMap R.Alt "W" $ R.rSpawn "firefox")
  , (R.riverNormalMap R.Super "Q" $ R.rClose)
  , (R.riverNormalMap R.SS "E" $ R.rExit)
  , (R.riverNormalMap R.Super "J" $ R.rFocusView R.Next)
  , (R.riverNormalMap R.Super "K" $ R.rFocusView R.Previous)
  , (R.riverNormalMap R.SS "J" $ R.rSwap R.Next)
  , (R.riverNormalMap R.SS "K" $ R.rSwap R.Previous)
  , (R.riverNormalMap R.SA "H" $ R.rMove R.Left "100")
  , (R.riverNormalMap R.SA "L" $ R.rMove R.Right "100")
  , (R.riverNormalMap R.SA "J" $ R.rMove R.Down "100")
  , (R.riverNormalMap R.SA "K" $ R.rMove R.Up "100")
  , (R.riverNormalMap R.Super "Print" $ R.rSpawn "'grim -g \"$(slurp)\" -| wl-copy && notify-send \"Screenshot Clipped\"'")
  , (R.riverNormalMapPointer R.Super "BTN_LEFT" $ R.rMoveView)
  , (R.riverNormalMapPointer R.Super "BTN_RIGHT" $ R.rResizeView)
  , (R.riverNormalMapPointer R.Super "BTN_MIDDLE" $ R.rToggleFloat)
  ]

main :: IO ()
main = do
  R.applyKeybinds initialKeymap
  R.riverCreateTags 9
  R.riverAllTags "0"
  for_ [R.riverHideCursor "timeout" "5000",R.riverHideCursor "when-typing" "enabled"] R.callRiver
  R.callRiver $ R.riverSetCursorWarp
  R.callRiver $ R.riverFocusFollowsCursor
  R.callRiver $ R.riverBackgroundColour "0x002b36"
  R.callRiver $ R.riverBorderColourFocused "0xC34A6A"
  R.callRiver $ R.riverBorderColourUnfocused "0x000000"
  R.callRiver $ R.riverSetRepeat "50" "250"
  R.callRiver $ R.riverDefaultLayout "bsp-layout"
  R.callExternal "river-bsp-layout" ["--inner-gap","5", "--outer-gap", "10", "--split-perc", "0.5"]
