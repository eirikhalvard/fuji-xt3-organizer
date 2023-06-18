module Cli where

import Options.Applicative
import Types

parseCommand :: IO Command
parseCommand = execParser opts

opts :: ParserInfo Command
opts =
    info
        ( hsubparser
            ( command "create" (info createParser (progDesc "Add a folder to the SSD"))
                <> command "transfer" (info transferParser (progDesc "Transfer files from the SD card to the specified location"))
                <> command "createAndTransfer" (info createAndTransferParser (progDesc "Create a folder on the SSD and transfer the files from the SD card"))
                <> command "info" (info informationParser (progDesc "Displays information"))
                <> command "gui" (info guiParser (progDesc "Launch the graphical user interface"))
                <> command "update" (info updateParser (progDesc "Updates photos export folders"))
                <> command "export" (info exportParser (progDesc "Show the export folder"))
            )
            <**> helper
        )
        ( fullDesc
            <> progDesc "Helps create folders and transfer photos between SD card and external SSD"
            <> header "Fujifilm X-T3 Transfer Script"
        )

createParser :: Parser Command
createParser =
    Create <$> createComponentParser

transferParser :: Parser Command
transferParser =
    Transfer <$> transferComponentParser

createAndTransferParser :: Parser Command
createAndTransferParser =
    CreateAndTransfer
        <$> createComponentParser
        <*> transferComponentParser

informationParser :: Parser Command
informationParser =
    pure Info

updateParser :: Parser Command
updateParser =
    pure UpdateFolders

guiParser :: Parser Command
guiParser = pure GUI

exportParser :: Parser Command
exportParser = pure ShowExport

createComponentParser :: Parser String
createComponentParser =
    strOption
        ( long "name"
            <> metavar "SESSION_NAME"
            <> help "Name of the photo session"
        )

transferComponentParser :: Parser Transfer
transferComponentParser =
    transferAllParser <|> transferRangeParser

transferAllParser :: Parser Transfer
transferAllParser =
    flag' AllTransfer (long "all" <> help "transfer all photos from SD card")

transferRangeParser :: Parser Transfer
transferRangeParser =
    RangeTransfer
        <$> option
            auto
            ( long "from"
                <> short 'F'
                <> metavar "INT"
                <> help "Number to start transfering from"
            )
        <*> option
            auto
            ( long "to"
                <> short 'T'
                <> metavar "INT"
                <> help "Number to start transfering to"
            )
