module Gui where

import qualified Graphics.Vty as V

import qualified Brick.AttrMap as A
import qualified Brick.BChan as BC
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (bg, clamp, fg, on)
import Brick.Widgets.Core (
  overrideAttr,
  str,
  updateAttrMap,
  (<+>),
  (<=>),
 )
import qualified Brick.Widgets.ProgressBar as P
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Types

runGui :: Env -> IO ()
runGui env = do
  eventChan <- BC.newBChan 10

  mainThreadId <- forkIO $ simulateTransfer eventChan 100 

  let initialState = 0.0
      buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  finalState <-
    M.customMain
      initialVty
      buildVty
      (Just eventChan)
      app
      initialState
  print finalState

type AppState = Float

data ApplicationEvent
  = TransferProgress Float

valid :: Float -> Float
valid = clamp (0.0 :: Float) 1.0

simulateTransfer :: BC.BChan ApplicationEvent -> Int -> IO ()
simulateTransfer chan num = mapM_ transferOne [1..num]
  where transferOne n = do
            threadDelay 20000 -- wait 0.2 per transfer
            let progress = TransferProgress (fromIntegral n / fromIntegral num)
            BC.writeBChan chan progress


app :: M.App AppState ApplicationEvent String
app =
  M.App
    { M.appDraw = guiDraw
    , M.appChooseCursor = guiChooseCursor
    , M.appHandleEvent = guiHandleEvent
    , M.appStartEvent = guiStartEvent
    , M.appAttrMap = guiAttrMap
    }

guiDraw :: AppState -> [T.Widget String]
guiDraw appState = [ui]
 where
  transferBar =
    updateAttrMap
      ( A.mapAttrNames
          [ (doneAttr, P.progressCompleteAttr)
          , (todoAttr, P.progressIncompleteAttr)
          ]
      )
      bar
  label = Just $ show (fromEnum $ appState * 100) ++ "%"
  bar = P.progressBar label appState
  ui =
    (str "Transfer progress: " <+> transferBar)
      <=> str ""
      <=> str "Hit 'j' or 'k' to advance progress, q to quit"

guiChooseCursor :: AppState -> [T.CursorLocation String] -> Maybe (T.CursorLocation String)
guiChooseCursor = const . const Nothing

guiHandleEvent :: AppState -> T.BrickEvent String ApplicationEvent -> T.EventM String (T.Next AppState)
guiHandleEvent appState (T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'j') [] -> M.continue $ valid (appState - 0.07)
    V.EvKey (V.KChar 'k') [] -> case appState + 0.07 of
      s | s >= 1 -> M.halt 42.0
      s | otherwise -> M.continue $ valid s
    V.EvKey (V.KChar 'q') [] -> M.halt (- 1.0)
    _ -> M.continue appState
guiHandleEvent appState (T.AppEvent (TransferProgress n)) = M.continue $ valid n
guiHandleEvent appState _ = M.continue appState

doneAttr, todoAttr :: A.AttrName
doneAttr = theBaseAttr <> A.attrName "X:done"
todoAttr = theBaseAttr <> A.attrName "X:remaining"

theBaseAttr :: A.AttrName
theBaseAttr = A.attrName "theBase"

guiStartEvent :: AppState -> T.EventM String AppState
guiStartEvent = return

guiAttrMap :: AppState -> A.AttrMap
guiAttrMap appState =
  A.attrMap
    V.defAttr
    [ (theBaseAttr, bg V.brightBlack)
    , (doneAttr, V.black `on` V.white)
    , (todoAttr, V.white `on` V.black)
    , (P.progressIncompleteAttr, fg V.yellow)
    ]
