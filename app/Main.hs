module Main where

import System.Environment
import Text.Regex
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import qualified Graphics.Vty as V
import qualified Data.Map as M
import Data.List (sort)
import Lang as L

data Name = ViewportMain
          | CursorMain
          deriving (Ord, Show, Eq)


app :: App L.World e Name
app = App {
    appDraw = drawApp,
    appChooseCursor = showFirstCursor,
    appHandleEvent = handleEvent,
    appStartEvent = return,
    appAttrMap = const $ attrMap V.defAttr []
  }

drawApp :: World -> [Widget Name]
drawApp w = [drawWindow w <=> drawModeline w]

cursorPos :: L.Buffer -> Location
cursorPos (s, p) = Location (col, row)
  where pred '\n' = True
        pred _ = False
        row = length $ filter pred $ take (p+1) s
        lastNewline pos str = if
          pos < length str && (pos <= 0 || (str !! pos == '\n'))
          then pos else lastNewline (pos - 1) str
        col = p - lastNewline p s

drawWindow :: L.World -> Widget Name
drawWindow (b@(s, p), _) = viewport ViewportMain Both
  $ showCursor CursorMain (cursorPos b)
  $ str s

drawModeline :: L.World -> Widget Name
drawModeline w = str "--*-"

encodeModifier :: V.Modifier -> String
encodeModifier V.MCtrl = "C-"
encodeModifier V.MMeta = "M-"
encodeModifier V.MShift = "S-"
encodeModifier V.MAlt = "A-"

encodeKey :: V.Event -> String
encodeKey (V.EvKey (V.KChar char) modifiers) =
  concat (fmap encodeModifier (sort modifiers)) ++ [char]
encodeKey _ = "???"

handleEvent :: World -> BrickEvent Name e -> EventM Name (Next World)
handleEvent w (VtyEvent key) = halt w
handleEvent w _ = continue w

main :: IO ()
main = do finalState <- defaultMain app L.initialWorld
          return ()
