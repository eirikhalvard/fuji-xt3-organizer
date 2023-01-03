module Gui where

import qualified Graphics.Vty as V

import qualified Brick.AttrMap as A
import qualified Brick.BChan as BC
import Brick.Main
import qualified Brick.Main as M
import Brick.Themes
import qualified Brick.Types as T
import Brick.Util (bg, clamp, fg, on)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import qualified Brick.Widgets.ProgressBar as P
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Graphics.Vty
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

app :: M.App AppState ApplicationEvent String
app =
  M.App
    { M.appDraw = guiDraw
    , M.appChooseCursor = guiChooseCursor
    , M.appHandleEvent = guiHandleEvent
    , M.appStartEvent = guiStartEvent
    , M.appAttrMap = guiAttrMap
    }

data Name = ViewportScroller deriving (Eq, Show)

guiDraw :: AppState -> [T.Widget String]
guiDraw appState = [ui]
 where
  (State env commandState quitable logList folderStatus) = appState
  ui =
    drawHeader appState
      <=> hCenter (drawMain commandState)
      <=> scrollerWidget logList
      <=> hCenter (padBottom (T.Pad 1) (quitableWidget quitable))

drawHeader :: AppState -> T.Widget String
drawHeader (State env commandState quitable logList folderStatus) =
  let fields =  hBox [text, status]
      text =
        vBox
          [ str "State: "
          , str "SD Card: "
          , str "SSD: "
          , str "Export: "
          ]
      status =
        vBox
          [ case quitable of
              IsQuitable -> statusStr Good "Program is finished ('q' to quit)"
              IsRunning -> statusStr InProgress "Program is running"
          , if sdStatus folderStatus
              then statusStr Good "Connected"
              else statusStr Bad "Not connected"
          , if ssdStatus folderStatus
              then statusStr Good "Connected"
              else statusStr Bad "Not connected"
          , if exportStatus folderStatus
              then statusStr Good "Exists"
              else statusStr Bad "Does not exist"
          ]
      xt3 =
        padLeftRight
          1
          ( vBox
              [ str "██╗  ██╗  ████████╗██████╗ "
              , str "╚██╗██╔╝  ╚══██╔══╝╚════██╗"
              , str " ╚███╔╝█████╗██║    █████╔╝"
              , str " ██╔██╗╚════╝██║    ╚═══██╗"
              , str "██╔╝ ██╗     ██║   ██████╔╝"
              , str "╚═╝  ╚═╝     ╚═╝   ╚═════╝ "
              ]
          )
   in hBox
        [ fields
        , padLeft T.Max xt3
        ]

data Status = Good | InProgress | Bad

statusStr :: Status -> String -> T.Widget String
statusStr status flavorText =
  let statusPart = case status of
        Good -> withAttr goodAttr (str "[✓] ")
        InProgress -> withAttr inProgressAttr (str "[~] ")
        Bad -> withAttr badAttr (str "[x] ")
      flavorPart = str flavorText
   in statusPart <+> flavorPart

drawMain (CreateState name) = str "Creating folders"
drawMain (TransferState transfer num) = progressWidget "Transfer progress: " num
drawMain (CreateAndTransferState name transfer num) = progressWidget "Create and transfer progress: " num
drawMain InfoState = str "Showing info"
drawMain GUIState = str "GUI Test"
drawMain UpdateFoldersState = str "Updating export folders"
drawMain ShowExportState = str "Showing export folder state"

quitableWidget IsQuitable = str "Program is finished! Press 'q' to exit"
quitableWidget IsRunning = str "Program is currently running"

scrollerWidget :: LogList -> T.Widget String
scrollerWidget logList =
  let content = vBox (str <$> logList)
      scroller = viewport "ViewportScroller" T.Vertical content
      withBars = withVScrollBars T.OnRight scroller
      withBorder = border withBars
   in withBorder

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
guiChooseCursor _ _ = Nothing

guiHandleEvent :: AppState -> T.BrickEvent String ApplicationEvent -> T.EventM String (T.Next AppState)
guiHandleEvent appState (T.AppEvent (TransferProgress n)) =
  M.continue $ updateProgress appState (valid n)
guiHandleEvent appState (T.AppEvent (AppendLogList entry)) =
  M.continue $ appendToLogList appState entry
guiHandleEvent (State env cmdState _ logList folderStatus) (T.AppEvent Finished) =
  M.continue (State env cmdState IsQuitable logList folderStatus)
guiHandleEvent (State env cmdState quitable logList folderStatus) (T.AppEvent (SDStatus status)) =
  M.continue (State env cmdState quitable logList (folderStatus { sdStatus = status }))
guiHandleEvent (State env cmdState quitable logList folderStatus) (T.AppEvent (SSDStatus status)) =
  M.continue (State env cmdState quitable logList (folderStatus { ssdStatus = status }))
guiHandleEvent (State env cmdState quitable logList folderStatus) (T.AppEvent (ExportStatus status)) =
  M.continue (State env cmdState quitable logList (folderStatus { exportStatus = status }))
guiHandleEvent appState (T.AppEvent Exit) = M.halt appState
guiHandleEvent appState (T.VtyEvent (V.EvKey k [])) = handleKeyPress appState k
guiHandleEvent appState _ = M.continue appState

handleKeyPress :: AppState -> V.Key -> T.EventM String (T.Next AppState)
handleKeyPress appState (V.KChar k) =
  case k of
    'q' -> maybeQuit appState
    'j' -> scrollWith (`vScrollBy` 1)
    'k' -> scrollWith (`vScrollBy` (-1))
    'd' -> scrollWith (`vScrollPage` T.Down)
    'u' -> scrollWith (`vScrollPage` T.Up)
    'G' -> scrollWith vScrollToEnd
    'g' -> scrollWith vScrollToBeginning
    'l' -> scrollWith (`hScrollBy` 1)
    'h' -> scrollWith (`hScrollBy` (-1))
    _ -> continue appState
 where
  scrollWith scroller =
    scroller (viewportScroll "ViewportScroller") >> continue appState
  maybeQuit (State _ _ IsQuitable _ _) = M.halt appState
  maybeQuit _ = M.continue appState
handleKeyPress appState _ = M.continue appState

updateProgress :: AppState -> Float -> AppState
updateProgress (State env (TransferState transfer _) q logList folderStatus) newNum =
  State env (TransferState transfer newNum) q logList folderStatus
updateProgress (State env (CreateAndTransferState name transfer _) q logList folderStatus) newNum =
  State env (CreateAndTransferState name transfer newNum) q logList folderStatus
updateProgress other _ = other

appendToLogList :: AppState -> String -> AppState
appendToLogList (State env cmdState q logList folderStatus) entry =
  State env cmdState q (entry : logList) folderStatus

doneAttr, todoAttr :: A.AttrName
doneAttr = theBaseAttr <> A.attrName "X:done"
todoAttr = theBaseAttr <> A.attrName "X:remaining"

goodAttr = A.attrName "goodStatusText"
inProgressAttr = A.attrName "inProgressStatusText"
badAttr = A.attrName "badStatusText"
boldAttr = A.attrName "bold"

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
    , (goodAttr, withStyle (fg V.green) bold)
    , (inProgressAttr, withStyle (fg V.yellow) bold)
    , (badAttr, withStyle (fg V.red) bold)
    ]
