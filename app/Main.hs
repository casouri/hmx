module Main where

import System.Environment
import Text.Regex
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import qualified Graphics.Vty as V
import qualified Data.Map as M
import Lang as L

data World = World Buffer L.Obarray

data Name = ViewportMain
          | CursorMain
          deriving (Ord, Show, Eq)

data Buffer = Buffer String Int

point :: Buffer -> Int
point (Buffer _ p) = p

goto :: Int -> Buffer -> Buffer
goto p (Buffer s op) = Buffer s p

cursorPos :: Buffer -> Location
cursorPos (Buffer s p) = Location (col, row)
  where pred '\n' = True
        pred _ = False
        row = length $ filter pred $ take (p+1) s
        lastNewline pos str = if
          pos < length str && (pos <= 0 || (str !! pos == '\n'))
          then pos else lastNewline (pos - 1) str
        col = p - lastNewline p s

app :: App World e Name
app = App {
    appDraw = drawApp,
    appChooseCursor = showFirstCursor,
    appHandleEvent = handleEvent,
    appStartEvent = return,
    appAttrMap = const $ attrMap V.defAttr []
  }

drawApp :: World -> [Widget Name]
drawApp w = [drawWindow w <=> drawModeline w]

drawWindow :: World -> Widget Name
drawWindow (World b@(Buffer s p) _) = viewport ViewportMain Both
  $ showCursor CursorMain (cursorPos b)
  $ str s

drawModeline :: World -> Widget Name
drawModeline w = str "--*-"

handleEvent :: World -> BrickEvent Name e -> EventM Name (Next World)
handleEvent w (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt w
handleEvent w _ = continue w

main :: IO ()
main = do finalState <- defaultMain app initState
          return ()
            where initState = World (Buffer "Welcome to HMX" 0)
                    L.initialObarray
