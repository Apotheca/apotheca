module Caligo.Runtime.Options
( RuntimeOptions (..)
, getOptions
, runOptions
) where


import           Options.Applicative

import           Caligo.Logs
import           Caligo.Repo.Path
import           Caligo.Repo.Repo
import           Caligo.Repo.Types

import           Caligo.Runtime.Commands



getOptions :: IO RuntimeOptions
getOptions = execParser $ parseOptions `withInfo` "Manage Caligo storage."

runOptions :: (RuntimeOptions -> IO ()) -> IO ()
runOptions r = getOptions >>= r

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts)
  $ fullDesc
  <> progDesc desc
  <> header "Caligo DHT - distributed data storage"
  <> footer "Goodbye."

data RuntimeOptions = Options {
    optSearchDir  :: Maybe FilePath
  , optExtDir     :: Maybe FilePath
  , optIntDir     :: Maybe Path
  , optMagicSlash :: Bool
  , optVerbosity  :: Maybe Verbosity
  , optCommand    :: RuntimeCommand
  } deriving (Show, Read, Eq)

parseOptions :: Parser RuntimeOptions
parseOptions = Options
  <$> parseRepoDir
  <*> parseExtDir
  <*> parseIntDir
  <*> parseMagicSlash
  <*> parseVerbosity
  <*> parseCommand

parseRepoDir :: Parser (Maybe FilePath)
parseRepoDir = optional $ strOption
  ( short 's'
  <> long "store-dir"
  <> metavar "STOREDIR"
  -- <> value "."
  <> help "Caligo store directory - the location of the store repository. This \
    \allows you to target a repo other than in the current directory / ancestry\
    \ If specified, it must be an exact path. If not specified, a recursive \
    \search upwards from the current working directory will be performed."
  )

parseExtDir :: Parser (Maybe FilePath)
parseExtDir = optional $ strOption
  ( short 'e'
  <> long "ext-dir"
  <> metavar "EXTDIR"
  -- <> value "."
  <> help "External working directory - source and destination for \
    \transactions. Files will be pushed-from and pulled-to relative to this \
    \directory. This allows you to specify paths relative to another directory. \
    \If not specified, will use the current directory."
  )

parseIntDir :: Parser (Maybe Path)
parseIntDir = optional $ (fromFilePath <$> strOption
  ( short 'i'
  <> long "int-dir"
  <> metavar "INTDIR"
  -- <> value "/"
  <> help "Internal working path - source and destination for \
    \transactions. Files will be pushed-to and pulled-from relative to this \
    \directory. If not specified: For a bare repo, defaults to root. For a hidden repo, defaults \
    \to the current directory relative to the store directory, or root if the \
    \current directory is not beneath the store directory."
  ))

parseMagicSlash :: Parser Bool
parseMagicSlash = flag True False (long "no-magic-slash" <> help "Disable magic slash")

-- TODO: Change this to a --verbosity silent/fatal/warn/verbose/debug
parseVerbosity :: Parser (Maybe Verbosity)
parseVerbosity = optional $ flag' Silent (long "silent" <> help "Run without any print output.")
  <|> flag' Fatal (long "fatal" <> help "Run with only fatal print output.")
  <|> flag' Warn (long "warn" <> help "Run with >= warning print output.")
  <|> flag' Verbose (long "verbose" <> help "Run with >= verbose print output.")
  <|> flag' Debug (long "debug" <> help "Run with all print output, including debug.")

parseCommand :: Parser RuntimeCommand
parseCommand = subparser
  -- Repo management
  ( subcmd "new" "Initialize a store." parseNew
  <> subcmd "nuke" "Nuke a store." parseNuke
  -- Query
  <> subcmd "where" "Find and print store directory, if it exists." (pure Where)
  <> subcmd "info" "Print store info." (pure Info)
  -- Map-like
  <> subcmd "list" "List files in a store." parseList
  <> subcmd "get" "Get a file from a store." parseGet
  <> subcmd "put" "Put a file into a store." parsePut
  <> subcmd "del" "Delete a file from a store." parseDel
  -- Sync
  <> subcmd "push" "Pushes a path into a store." parsePush
  <> subcmd "pull" "Pulls a path from a store." parsePull
  <> subcmd "transfer" "Transfers a path between stores." parseTransfer
  -- Watch
  <> subcmd "watch" "Adds a directory to the watchlist." parseWatch
  <> subcmd "unwatch" "Removes a directory from the watchlist." parseUnwatch
  -- Node
  <> subcmd "run" "Run a distributed node." parseRunNode
  ) <|> pure NoCommand



-- Subcommands

subcmd :: String -> String -> Parser RuntimeCommand -> Mod CommandFields RuntimeCommand
subcmd cmd info parser = command cmd $ parser `withInfo` info



-- Repo management

parseNew :: Parser RuntimeCommand
parseNew = New <$> (switch $
  ( long "bare"
  <> help "Create as a bare repository."
  ))

parseNuke :: Parser RuntimeCommand
parseNuke = Nuke <$> parseForce "Force removal without confirmation, ignoring errors."



-- Query


-- Map-like

parseList = List
  <$> parseRecurse "List directories recursively."
  <*> parseIntPath

parseGet = Get
  <$> parseOverwrite "Overwrite existing files."
  <*> parseReplace "Replace directories instead of merging."
  <*> parseRecurse "Recurse over directory contents."
  <*> parseIntPath -- "Source file or directory in store."
  <*> parseExtPath -- "Dest file or directory in store."

parsePut = Put
  <$> parseOverwrite "Overwrite existing files."
  <*> parseReplace "Replace directories instead of merging."
  <*> parseRecurse "Recurse over directory contents."
  <*> parseExtPath -- "Source file or directory in store."
  <*> parseIntPath -- "Dest file or directory in store."

parseDel = Del
  <$> parseForce "Force deletion without confirmation, ignoring errors."
  -- <*> parseRecurse "Delete children recursively."
  <*> parseIntPath -- "Target path to delete."



-- Sync strategies

parsePush = Push
  <$> parseWatchMode

parsePull = Pull
  <$> parseWatchMode

parseTransfer = Transfer
  <$> parseWatchMode



-- Watch

parseWatch = pure Watch

parseUnwatch = pure Unwatch



-- Run

parseRunNode = pure RunNode



-- Helpers

-- parsePaths :: Parser [Path]
-- parsePaths =  map fromFilePath <$> some (argument str (metavar "SRCS"))

parseExtPath :: Parser FilePath
parseExtPath = strArgument
  ( metavar "EXT-PATH"
  -- <> value "."
  <> help "An external path; if it is a relative path, is relative to EXT-DIR."
  )

parseIntPath :: Parser FilePath
parseIntPath = strArgument
  ( metavar "INT-PATH"
  -- <> value "/"
  <> help "An internal path; if it is a relative path, is relative to INT-DIR."
  )

parseGlob :: Parser (Maybe Glob)
parseGlob = optional $ strOption
  ( short 'g'
  <> long "glob"
  <> metavar "GLOB"
  <> help "Use glob pattern matching"
  )

parseForce :: String -> Parser Bool
parseForce s = switch $
  ( short 'f'
  <> long "force"
  <> help s
  )

parseOverwrite :: String -> Parser Bool
parseOverwrite s = switch $
  ( short 'o'
  <> long "overwrite" -- maybe should be overwrite-files
  <> help s
  )

parseReplace :: String -> Parser Bool
parseReplace s = switch $
  ( short 'p'
  <> long "replace" -- maybe should be replace-dirs
  <> help s
  )

parseRecurse :: String -> Parser Bool
parseRecurse s = switch $
  ( short 'r'
  <> long "recurse" -- maybe should be recurse-children
  <> help s
  )

parseWatchMode :: Parser WatchMode
parseWatchMode = flag' DeadDropMode (long "deaddrop" <> help "Run with only fatal print output.")
  <|> flag' (AdditiveMode True) (long "addover" <> help "Additive mode, with overwrite.")
  <|> flag' (AdditiveMode False) (long "additive" <> help "Additive mode, no overwrite.")
  <|> flag SynchronizeMode SynchronizeMode (long "synchronize" <> help "Synchronize mode")