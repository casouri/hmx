module Main where

import System.Environment
import System.Directory
import Text.Regex
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import qualified Graphics.Vty as V
import qualified Data.Map as M
import Data.List (sort, intersperse)
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
        lastNewlinePos pos str =
          -- POS cannot be greater than buffer length.
          if pos < 0 || (str !! pos == '\n')
          then pos + 1
          else lastNewlinePos (pos - 1) str
        col = p - lastNewlinePos (p-1) s

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
encodeKey' (V.KChar '\\') = "\\\\"
encodeKey' (V.KChar '"') = "\\\""
encodeKey' (V.KChar char) = [char]
encodeKey' V.KEnter = "RET"
encodeKey' V.KEsc = "ESC"
encodeKey' V.KBS = "DEL"
encodeKey' _ = "???"

printStack :: Expression -> String
printStack (ExprList lst) = concat $ intersperse "\n"  $map prin1 lst
printStack x = prin1 x

handleEvent :: World -> BrickEvent Name e -> EventM Name (Next World)
handleEvent w (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt w
handleEvent w@(obarray, minibuffer) (VtyEvent key) =
  let result = evalS obarray
        ("(run-command " ++ "\"" ++ encodeKey key ++ "\"" ++ ")") in
    case result of
      Left (exp, err) -> continue (obarray, err ++ ": " ++ printStack exp)
      Right (ExprSymbol "halt", _) -> halt w
      Right (exp, newObarray) -> continue (newObarray, encodeKey key)
handleEvent w _ = continue w

main :: IO ()
main = do args <- getArgs
          contents <- if length args == 1
                      then readFile (head args)
                      else return ""
          v <- evalFileS "setup.hmx"
               [M.fromList [("write-content", ExprString contents),
                            ("__stackframe", nil)]]
          case v of
            Left (exp, err) -> putStrLn (err ++ ": " ++ prin1 exp)
            Right (exp, obarray) ->
              do (ob, mini) <- defaultMain app (obarray, "")
                 case M.lookup "write-content" (head ob) of
                   Just (ExprString str) ->
                     if length args == 1
                     then writeFile (head args) str
                     else return ()
                   _ -> return ()
          
