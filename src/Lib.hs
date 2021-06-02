module Lib (someFunc) where

import Data.Char (isSpace, toLower, toUpper)
import Data.Time.Clock
import Data.Time.Format
import System.Directory

t5Lib :: FilePath
t5Lib = "/Volumes/EirikT5/Pictures/Fuji/"

getYear :: IO String
getYear = formatTime defaultTimeLocale "%Y" <$> getCurrentTime

getFolderName :: String -> IO String
getFolderName name = do
  prefix <- formatTime defaultTimeLocale "%Y-%m-%d | " <$> getCurrentTime
  return $ prefix ++ name

someFunc :: IO ()
someFunc = do
  -- set directory to the external library
  setCurrentDirectory t5Lib

  -- set directory to the right year
  year <- getYear
  let yearDir = "./" ++ year
  createDirectoryIfMissing False yearDir
  setCurrentDirectory yearDir

  -- create folder structure
  name <- capitalize . trim <$> getLine
  folderName <- getFolderName name
  createDirectory folderName
  createDirectory $ folderName ++ "/" ++ "01_JPG"
  createDirectory $ folderName ++ "/" ++ "02_RAW"
  createDirectory $ folderName ++ "/" ++ "03_EXPORT"

  print "listing dirs..."
  dirs <- listDirectory "."
  mapM_ print dirs

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

capitalize :: String -> String
capitalize "" = ""
capitalize (x : xs) = toUpper x : fmap toLower xs
