module Types where

import qualified Brick.BChan as BC

data Command
  = Create String
  | Transfer Transfer
  | CreateAndTransfer String Transfer
  | Info
  | GUI
  | UpdateFolders
  | ShowExport
  deriving (Show, Eq)

data Transfer
  = RangeTransfer Int Int
  | AllTransfer
  deriving (Show, Eq)

data Env = Env
  { sdLib :: FilePath
  , ssdLib :: FilePath
  , ssdBaseLib :: FilePath
  , exportLib :: FilePath
  , jpgFolderName :: String
  , rawFolderName :: String
  , exportFolderName :: String
  , movieFolderName :: String
  , year :: String
  , folderName :: Maybe String
  , canonicalFilenamePrefix :: String
  , eventChan :: BC.BChan ApplicationEvent
  }

data Extension = Jpg String | Raw String | Export String | Movie String

data AppState = State Env CommandState Quitable LogList FolderStatus

data FolderStatus = FolderStatus { 
sdStatus :: Bool, ssdStatus :: Bool 
  , exportStatus :: Bool
                                 }

data Quitable = IsQuitable | IsRunning

type LogList = [String]

data CommandState
  = CreateState String
  | TransferState Transfer Float
  | CreateAndTransferState String Transfer Float
  | InfoState
  | GUIState
  | UpdateFoldersState
  | ShowExportState

data ApplicationEvent
  = TransferProgress Float
  | AppendLogList String
  | SDStatus Bool
  | SSDStatus Bool
  | ExportStatus Bool
  | Finished
  | Exit
