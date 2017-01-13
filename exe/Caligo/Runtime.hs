module Caligo.Runtime
( execRuntime
) where

import           Options.Applicative

import           System.Directory
import           System.FilePath
import           System.IO

execRuntime :: IO ()
execRuntime = do
  setupConsole
  opts <- getOptions
  putStrLn $ concat [ "Options were: ", show opts ]
  -- (repo, cmd) <- getState opts
  -- runCmd $ optCommand opts

setupConsole = do
  hSetBuffering stdout NoBuffering

getOptions :: IO RuntimeOptions
getOptions = execParser $ parseOptions `withInfo` "Manage Caligo storage."

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts)
  $ fullDesc
  <> progDesc desc
  <> header "Caligo DHT - distributed data storage"
  <> footer "Goodbye."

data RuntimeOptions = Options {
  optRepoDir :: Maybe FilePath
, optCommand :: RuntimeCommand
} deriving (Show, Read, Eq)

data RuntimeCommand
  = NoCommand
  -- Repo management
  | New
  | Nuke Bool
  | Info
  -- Map-like
  | Get
  | Put
  | Del
  | List
  deriving (Show, Read, Eq)

parseOptions :: Parser RuntimeOptions
parseOptions = Options
  <$> parseStoreDir
  <*> parseCommand

parseStoreDir :: Parser (Maybe FilePath)
parseStoreDir = optional $ strOption
  ( short 's'
  <> long "store"
  <> metavar "STORE"
  <> help "Caligo store directory"
  )

-- makeCommand :: String -> String -> Parser RuntimeCommand -> Parser RuntimeCommand
makeCommand :: String -> String -> Parser RuntimeCommand -> Mod CommandFields RuntimeCommand
makeCommand cmd info parser = command cmd $ parser `withInfo` info

parseCommand :: Parser RuntimeCommand
parseCommand = subparser
  ( makeCommand "new" "Initialize a store." (pure New)
  <> makeCommand "nuke" "Nuke a store." parseNuke
  <> makeCommand "info" "Print store environment info." (pure Info)
  <> makeCommand "get" "Get a file from storage." (pure Get)
  <> makeCommand "put" "Put a file into storage." (pure Put)
  <> makeCommand "del" "Delete a file from storage." (pure Del)
  <> makeCommand "list" "List files in storage." (pure List)
  ) <|> pure NoCommand

parseNuke :: Parser RuntimeCommand
parseNuke = Nuke
  <$> switch
    ( short 'f'
    <> long "force"
    <> help "Nuke without confirming."
    )
