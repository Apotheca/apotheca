module Apotheca.Runtime.Options
( RuntimeOptions (..)
, getOptions
, runOptions
) where


import           Options.Applicative

import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BC

import           Apotheca.Encodable        (GzipCompression (..))
import           Apotheca.Logs
import           Apotheca.Repo.Internal
import           Apotheca.Repo.Monad
import           Apotheca.Repo.Path
import           Apotheca.Security.Cipher
import           Apotheca.Security.Hash

import           Apotheca.Runtime.Commands



getOptions :: IO RuntimeOptions
getOptions = execParser $ parseOptions `withInfo` "Manage Apotheca storage."

runOptions :: (RuntimeOptions -> IO ()) -> IO ()
runOptions r = getOptions >>= r

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts)
  $ fullDesc
  <> progDesc desc
  <> header "Apotheca DHT - distributed encrypted data storage"
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
  <|> flag' Terse (long "terse" <> help "Run with >= terse print output. Default.")
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
  -- <> subcmd "push" "Pushes a path into a store." parsePush
  -- <> subcmd "pull" "Pulls a path from a store." parsePull
  -- <> subcmd "transfer" "Transfers a path between stores." parseTransfer
  -- Watch
  -- <> subcmd "watch" "Adds a directory to the watchlist." parseWatch
  -- <> subcmd "unwatch" "Removes a directory from the watchlist." parseUnwatch
  -- Node
  -- <> subcmd "run" "Run a distributed node." parseRunNode
  -- Non-repo commands
  <> subcmd "version" "Print version info." (pure Version)
  ) <|> pure NoCommand



-- Subcommands

subcmd :: String -> String -> Parser RuntimeCommand -> Mod CommandFields RuntimeCommand
subcmd cmd info parser = command cmd $ parser `withInfo` info



-- Repo management

parseNew :: Parser RuntimeCommand
parseNew = New <$> parseConfigFlags

parseConfigFlags :: Parser ConfigFlags
parseConfigFlags = ConfigFlags
  <$> switch (long "bare" <> help "Create as a bare repository.")
  <*> optional parseSplitStrat
  <*> optional (option auto (short 'l' <> long "large" <> help "Large file limit."))
  <*> (parseMaybeHashStrat Nothing <|> pure Nothing)
  <*> optional parseGzipCompression
  <*> (parseMaybeCipherStrat <|> pure Nothing)

parseNuke :: Parser RuntimeCommand
parseNuke = Nuke <$> parseForce "Force removal without confirmation, ignoring errors."



-- Query


-- Map-like

parseList = List
  <$> parseRecurse "List directories recursively."
  <*> pure False -- <*> parseTree "Print results as tree."
  <*> parseIntPath

parseGet = Get
  -- <$> parseStdout "Print file to stdout. Ignores -orp, EXT-PATH."
  -- <$> parseOverwrite "Overwrite existing files."
  <$> parseGetFlags
  <*> parseReplace "Replace directories instead of merging."
  <*> parseRecurse "Recurse over directory contents."
  <*> parseIntPath -- "Source file or directory in store."
  <*> parseExtPath -- "Dest file or directory in store."

parsePut = Put
  -- <$> parseStdin "Input file from stdin. Ignores EXT-PATH, implies -o." -- implies /currently/
  -- <$> parseOverwrite "Overwrite existing files."
  <$> parsePutFlags
  <*> parseReplace "Replace directories instead of merging."
  <*> parseRecurse "Recurse over directory contents."
  <*> parseExtPath -- "Source file or directory in store."
  <*> parseIntPath -- "Dest file or directory in store."

parseDel = Del
  <$> parseForce "Force deletion without confirmation, ignoring errors."
  -- <*> parseRecurse "Delete children recursively."
  <*> parseIntPath -- "Target path to delete."



-- Sync strategies

parsePush :: Parser RuntimeCommand
parsePush = SyncPush
  <$> parseSyncMode
  <*> parseGlob
  <*> parseExtPath
  <*> parseIntPath

parsePull :: Parser RuntimeCommand
parsePull = SyncPull
  <$> parseSyncMode
  <*> parseGlob
  <*> parseExtPath
  <*> parseIntPath

-- parseTransfer = Transfer
--   <$> parseSyncMode



-- Watch

parseWatch :: Parser RuntimeCommand
parseWatch = pure Watch

parseUnwatch :: Parser RuntimeCommand
parseUnwatch = pure Unwatch



-- Run
parseRunNode :: Parser RuntimeCommand
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

-- parseStdin :: String -> Parser Bool
-- parseStdin s = switch $
--   ( short 'x'
--   <> long "stdin"
--   <> help s
--   )
--
-- parseStdout :: String -> Parser Bool
-- parseStdout s = switch $
--   ( short 'x'
--   <> long "stdout"
--   <> help s
--   )

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
  ( long "tree"
  <> help s
  )

parseSyncMode :: Parser SyncMode
parseSyncMode = flag' DeadDropMode (long "deaddrop" <> help "Deletes source after transaction.")
  -- <|> flag' (AdditiveMode True) (long "addover" <> help "Additive mode, with overwrite.")
  <|> flag' AdditiveMode (long "additive" <> help "Additive mode, no overwrite.")
  <|> flag' SynchronizeMode (long "synchronize" <> help "Synchronize mode; DEFAULT")
  <|> pure SynchronizeMode

parseGzipCompression :: Parser GzipCompression
parseGzipCompression = flag' FastestCompression (long "gzip-fast" <> help "Compress with gzip (fast).")
  <|> flag' BestCompression (short 'Z' <> long "gzip-best" <> help "Compress with gzip (best).")
  <|> flag' BalancedCompression (short 'z' <> long "gzip" <> help "Compress with gzip (balanced).")
  <|> flag' NoCompression (long "no-gzip" <> help "Do not compress.")

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


prefixLong :: Maybe String -> String -> Mod OptionFields a
prefixLong mpf lng = long $ maybe lng (\pf -> concat [pf,"-",lng]) mpf

parseSplitStrat :: Parser SplitStrategy
parseSplitStrat
  = flag' NoSplit (long "no-split" <> help "Do not split into blocks.")
  <|> ConstSplit <$> option auto (long "const-split" <> help "Split into blocks of constant size.")
  <|> flag' (curry AdaptiveSplit) (long "adapt-split" <> help "Split into blocks of adaptive size.")
    <*> option auto (short 'M' <> long "adapt-min" <> value 4096 <> help "Minimum adaptive split block size. Default: 4kb")
    <*> option auto (short 'N' <> long "adapt-max" <> value 4194304 <> help "Maximum adaptive split block size. Default: 4mb")
  -- TODO: Write a parser for (Int,Int), then use this instead:
  -- <|> AdaptiveSplit <$> option auto (long "adapt-split" <> help "Split into blocks of constant size.")

-- NOTE: Having no default value on the first option makes this work right with
--  flags and Maybe HashStrategy
--  eg, "foo --hash Tiger == Just HashStrategy; "foo" == Nothing
--  If a default value is given, it is always a Just HashStrategy
parseHashStrat :: Maybe String -> Parser HashStrategy
parseHashStrat mprefix = HashStrategy
    <$> option auto (pflong "hash" <> metavar "HASH" <> help "Hash algorithm.")
    <*> (fmap BC.pack $ strOption (pflong "salt" <> metavar "SALT" <> value [] <> help "Salt."))
    <*> optional (option auto (pflong "hlimit" <> metavar "HLIMIT" <> help "Hash limit."))
  where
    pflong = prefixLong mprefix

parseMaybeHashStrat :: Maybe String -> Parser (Maybe HashStrategy)
parseMaybeHashStrat mprefix = (Just <$> parseHashStrat mprefix)
  <|> flag' Nothing (long "no-hash" <> help "No hash.")

parseCipherStrat :: Parser CipherStrategy
parseCipherStrat = CipherStrategy
    <$> option auto (long "cipher" <> metavar "CIPHER" <> help "Cipher algorithm.")
    -- <*> switch (long "derive-key" <> help "Derive an appropriate key from the secret.")
    <*> pure True -- Force derived keys for now, until error checking is in place
    <*> optional (parseHashStrat $ Just "ct")

parseMaybeCipherStrat :: Parser (Maybe CipherStrategy)
parseMaybeCipherStrat =
  (Just <$> parseCipherStrat)
  <|> flag' Nothing (long "no-cipher" <> help "No cipher.")

parsePutFlags :: Parser PutFlags
parsePutFlags = PutFlags
  <$> parseWriteMode
  -- <*> optional (option auto (long "mtime" <> metavar "TIME" <> help "Modification timestamp."))
  <*> inheritable parseSplitStrat
  <*> inheritable (parseMaybeHashStrat Nothing)
  <*> inheritable parseGzipCompression
  <*> inheritable  parseMaybeCipherStrat

inheritable :: Parser a -> Parser (Inherited a)
inheritable f = Explicit <$> f <|> pure Inherit


parseGetFlags :: Parser GetFlags
parseGetFlags = GetFlags
  <$> parseWriteMode
