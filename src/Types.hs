module Types where

data Command
  = Create String
  | Transfer Transfer
  | CreateAndTransfer String Transfer
  | Info
  | GUI
  | ShowExport
  deriving (Show, Eq)

data Transfer
  = RangeTransfer Int Int
  | AllTransfer
  deriving (Show, Eq)

data Env = Env
  { sdLib :: FilePath
  , ssdLib :: FilePath
  , exportLib :: FilePath
  , jpgFolderName :: String
  , rawFolderName :: String
  , exportFolderName :: String
  , movieFolderName :: String
  , year :: String
  , folderName :: Maybe String
  , canonicalFilenamePrefix :: String
  }
  deriving (Show, Eq)

