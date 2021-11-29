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

type World = (L.Obarray, String)

app :: App World e Name
app = App {
    appDraw = drawApp,
    appChooseCursor = showFirstCursor,
    appHandleEvent = handleEvent,
    appStartEvent = return,
    appAttrMap = const $ attrMap V.defAttr []
  }

drawApp :: World -> [Widget Name]
drawApp w = [drawWindow w <=> drawMinibuffer w]

cursorPos :: String -> Int -> Location
cursorPos s p = Location (col, row)
  where pred '\n' = True
        pred _ = False
        row = length $ filter pred $ take p s
        lastNewline pos str =
          if pos < length str
          then case pos of
                 pos | pos <= 0 -> 0
                 pos | str !! pos == '\n' -> pos + 1
                 _ -> lastNewline (pos - 1) str
          else lastNewline (pos - 1) str
        col = p - lastNewline (p-1) s

drawWindow :: World -> Widget Name
drawWindow (obarray, minibuffer) =
  case (evalS obarray "(buffer-string)",
        evalS obarray "(point)") of
    (Right (L.ExprString body, _),
     Right (L.ExprInt p, _)) ->
      viewport ViewportMain Both
      $ showCursor CursorMain (cursorPos body p)
      $ str body
    _ -> undefined

drawMinibuffer :: World -> Widget Name
drawMinibuffer (ob, minibuffer) =
  case evalS ob "(minibuffer-content)" of
    Right (L.ExprString txt, ob) -> case txt of
      "" -> str minibuffer
      _ -> str txt
    _ -> str minibuffer

encodeModifier :: V.Modifier -> String
encodeModifier V.MCtrl = "C-"
encodeModifier V.MMeta = "M-"
encodeModifier V.MShift = "S-"
encodeModifier V.MAlt = "A-"

encodeKey :: V.Event -> String
encodeKey (V.EvKey key modifiers) =
  concat (fmap encodeModifier (sort modifiers)) ++ encodeKey' key
encodeKey _ = "???"

encodeKey' :: V.Key -> String
encodeKey' (V.KChar char) = [char]
encodeKey' V.KEnter = "RET"
encodeKey' V.KEsc = "ESC"
encodeKey' V.KBS = "DEL"
encodeKey' _ = "???"

handleEvent :: World -> BrickEvent Name e
  -> EventM Name (Next World)
handleEvent w (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt w
handleEvent (obarray, minibuffer) (VtyEvent key) =
  let result = evalS obarray
        ("(run-command " ++ "\"" ++ (encodeKey key) ++ "\"" ++ ")") in
    case result of
      Left (exp, err) -> continue (obarray, err ++ ": " ++ prin1 exp)
      Right (exp, newObarray) -> continue (newObarray, encodeKey key)
handleEvent w _ = continue w

main :: IO ()
main = do v <- L.evalFileS "init.hmx"
          finalState <-
            case v of
              Left (exp, err) -> putStrLn err
              Right (exp, obarray) ->
                do finalState <- defaultMain app (obarray, "")
                   return ()
          return ()
          
