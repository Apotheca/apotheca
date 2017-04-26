module Apotheca.Runtime.Options
( RuntimeOptions (..)
, getOptions
, runOptions
) where


import           Options.Applicative

import           Apotheca.Logs
import           Apotheca.Repo.Path
import           Apotheca.Repo.Repo
import           Apotheca.Repo.Types

import           Apotheca.Runtime.Commands



getOptions :: IO RuntimeOptions
getOptions = execParser $ parseOptions `withInfo` "Manage Apotheca storage."

runOptions :: (RuntimeOptions -> IO ()) -> IO ()
runOptions r = getOptions >>= r

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts)
  $ fullDesc
  <> progDesc desc
  <> header "Apotheca DHT - distributed data storage"
  <> footer "Goodbye."

data RuntimeOptions = Options {
    optSearchDir  :: Maybe FilePath
  , optExtDir     :: Maybe FilePath
  , optIntDir     :: Maybe Path
  , optMagicSlash :: Bool
  , optVerbosity  :: Maybe Verbosity
  -- Command (always last)
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
  <> metavar "STORE-DIR"
  -- <> value "."
  <> help "Apotheca store directory."
  -- <> help "Apotheca store directory - the location of the store repository. This \
  --   \allows you to target a repo other than in the current directory / ancestry\
  --   \ If specified, it must be an exact path. If not specified, a recursive \
  --   \search upwards from the current working directory will be performed."
  )

parseExtDir :: Parser (Maybe FilePath)
parseExtDir = optional $ strOption
  ( short 'e'
  <> long "ext-dir"
  <> metavar "EXT-DIR"
  -- <> value "."
  <> help "External working directory."
  -- <> help "External working directory - source and destination for \
  --   \transactions. Files will be pushed-from and pulled-to relative to this \
  --   \directory. This allows you to specify paths relative to another directory. \
  --   \If not specified, will use the current directory."
  )

parseIntDir :: Parser (Maybe Path)
parseIntDir = optional $ (fromFilePath <$> strOption
  ( short 'i'
  <> long "int-dir"
  <> metavar "INT-DIR"
  -- <> value "/"
  <> help "Internal working directory."
  -- <> help "Internal working directory - source and destination for \
  --   \transactions. Files will be pushed-to and pulled-from relative to this \
  --   \directory. If not specified: For a bare repo, defaults to root. For a hidden repo, defaults \
  --   \to the current directory relative to the store directory, or root if the \
  --   \current directory is not beneath the store directory."
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
  -- <> subcmd "transfer" "Transfers a path between stores." parseTransfer
  -- Watch
  <> subcmd "watch" "Adds a directory to the watchlist." parseWatch
  <> subcmd "unwatch" "Removes a directory from the watchlist." parseUnwatch
  -- Node
  <> subcmd "run" "Run a distributed node." parseRunNode
  -- Non-repo commands
  <> subcmd "version" "Print version info." (pure Version)
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
  <*> parseTree "Print results as tree."
  <*> parseIntPath

parseGet = Get
  -- <$> parseStdout "Print file to stdout. Ignores -orp, EXT-PATH."
  <$> parseOverwrite "Overwrite existing files."
  <*> parseReplace "Replace directories instead of merging."
  <*> parseRecurse "Recurse over directory contents."
  <*> parseIntPath -- "Source file or directory in store."
  <*> parseExtPath -- "Dest file or directory in store."

parsePut = Put
  -- <$> parseStdin "Input file from stdin. Ignores EXT-PATH, implies -o." -- implies /currently/
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

parsePush = SyncPush
  <$> parseSyncMode
  <*> parseGlob
  <*> parseExtPath
  <*> parseIntPath

parsePull = SyncPull
  <$> parseSyncMode
  <*> parseGlob
  <*> parseExtPath
  <*> parseIntPath

-- parseTransfer = Transfer
--   <$> parseSyncMode



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
  <> help "An external path; if it is a relative path, is relative to EXT-DIR. \
    \With files, '-' may be used to specify stdin/stdout as source / destination."
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

parseStdin :: String -> Parser Bool
parseStdin s = switch $
  ( short 'x'
  <> long "stdin"
  <> help s
  )

parseStdout :: String -> Parser Bool
parseStdout s = switch $
  ( short 'x'
  <> long "stdout"
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

parseTree :: String -> Parser Bool
parseTree s = switch $
  ( short 't'
  <> long "tree"
  <> help s
  )

parseSyncMode :: Parser SyncMode
parseSyncMode = flag' DeadDropMode (long "deaddrop" <> help "Deletes source after transaction.")
  -- <|> flag' (AdditiveMode True) (long "addover" <> help "Additive mode, with overwrite.")
  <|> flag' AdditiveMode (long "additive" <> help "Additive mode, no overwrite.")
  <|> flag' SynchronizeMode (long "synchronize" <> help "Synchronize mode; DEFAULT")
  <|> pure SynchronizeMode

parseWriteMode :: Parser WriteMode
parseWriteMode
  = flag' Add
    ( short 'a'
    <> long "add"
    <> help "Add if non-existent, ignore if existent. Default.")
  <|> flag' Overwrite
    ( short 'o'
    <> long "overwrite"
    <> help "Add if non-existent, overwrite if existent.")
  <|> flag' Update
    ( short 'u'
    <> long "update"
    <> help "Add if nonexistent, overwrite if more recent.")
  <|> flag' Freshen
    ( short 'e'
    <> long "freshen"
    <> help "Ignore if non-existent, overwrite if more recent.")
  <|> pure Add
