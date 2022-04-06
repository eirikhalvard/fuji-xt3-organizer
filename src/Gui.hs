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
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Types

runGui :: BC.BChan ApplicationEvent -> AppState -> IO () -> IO ()
runGui eventChan initialState program = do
  mainThreadId <- forkIO program

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  finalState <-
    M.customMain
      initialVty
      buildVty
      (Just eventChan)
      app
      initialState
  return ()

valid :: Float -> Float
valid = clamp (0.0 :: Float) 1.0

simulateTransfer :: BC.BChan ApplicationEvent -> Int -> IO ()
simulateTransfer chan num = mapM_ transferOne [1 .. num]
 where
  transferOne n = do
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
guiDraw (State env commandState _) = [ui]
 where
  ui = case commandState of
    CreateState name ->
      str "create ..."
    TransferState transfer num ->
      str "transfer ..."
        <=> progressWidget "Transfer progress: " num
    CreateAndTransferState name transfer num ->
      str "create and transfer ..."
        <=> progressWidget "Transfer progress: " num
    InfoState ->
      str "info ..."
    GUIState ->
      str "gui ..."
    UpdateFoldersState clean ->
      str "updating folders ..."
    ShowExportState ->
      str "show export ..."

progressWidget :: String -> Float -> T.Widget String
progressWidget progressName progress = ui
 where
  transferBar =
    updateAttrMap
      ( A.mapAttrNames
          [ (doneAttr, P.progressCompleteAttr)
          , (todoAttr, P.progressIncompleteAttr)
          ]
      )
      bar
  label = Just $ show (fromEnum $ progress * 100) ++ "%"
  bar = P.progressBar label progress
  ui = str progressName <+> transferBar

guiChooseCursor :: AppState -> [T.CursorLocation String] -> Maybe (T.CursorLocation String)
guiChooseCursor = const . const Nothing

guiHandleEvent :: AppState -> T.BrickEvent String ApplicationEvent -> T.EventM String (T.Next AppState)
guiHandleEvent appState (T.AppEvent (TransferProgress n)) =
  M.continue $ updateProgress appState (valid n)
guiHandleEvent (State env cmdState _) (T.AppEvent Finished) = 
  M.continue (State env cmdState IsQuitable)
guiHandleEvent appState (T.AppEvent Exit) = M.halt appState
guiHandleEvent appState@(State _ _ IsQuitable) (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt appState
guiHandleEvent appState _ = M.continue appState

updateProgress :: AppState -> Float -> AppState
updateProgress (State env (TransferState transfer _) q) newNum =
  State env (TransferState transfer newNum) q
updateProgress (State env (CreateAndTransferState name transfer _) q) newNum =
  State env (CreateAndTransferState name transfer newNum) q
updateProgress other _ = other

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
