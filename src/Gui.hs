module Gui where

import Brick
import Brick.Widgets.ProgressBar
import Types
import Graphics.Vty (defAttr)

runGui :: Env -> IO ()
runGui env = do
  let initialState = 0.2
  defaultMain app initialState >>= print

type AppState = Float

app :: App AppState () String
app =
  App
    { appDraw = pure . progressBar (Just "Transfering files") :: AppState -> [Widget String]
    , appChooseCursor =
        const . const Nothing :: AppState -> [CursorLocation String] -> Maybe (CursorLocation String)
    , appHandleEvent =
        (\appState _ -> Brick.continue appState) :: AppState -> BrickEvent String () -> EventM String (Next AppState)
    , appStartEvent = return :: AppState -> EventM String AppState
    , appAttrMap = const (attrMap defAttr []) :: AppState -> AttrMap
    }

