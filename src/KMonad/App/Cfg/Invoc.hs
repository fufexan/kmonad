-- |

module KMonad.App.Cfg.Invoc

where

import KMonad.Prelude hiding (argument)

import KMonad.App.TypesNew
import KMonad.App.Cfg.Types


-- Imports required to add versioner to command
import KMonad.Args.TH (gitHash)
import Paths_kmonad (version)
import Data.Version (showVersion)

import Options.Applicative



-- | Parse 'Invoc' from the evocation of this program
getInvoc :: MonadIO m => m Invoc
getInvoc = liftIO . customExecParser (prefs showHelpOnEmpty) $
  info (invocP <**> versioner <**> helper)
    (  fullDesc
    <> progDesc "Start KMonad"
    <> header   "kmonad - an onion of buttons."
    )

-- | Equip a parser with version information about the program
versioner :: Parser (a -> a)
versioner = infoOption (showVersion version <> ", commit " <> $(gitHash))
  (  long "version"
  <> short 'V'
  <> help "Show version"
  )

--------------------------------------------------------------------------------
-- $prs
--
-- The different command-line parsers

type PM a = Parser (Maybe a)

-- | An option that parser "on" to True and "off" to False.
--
-- NOTE: We do this because we are trying to avoid *all* default values here. If
-- we were to use a switch, these default to 'off', so there would be no way to
-- distinguish set-to-off from default-off. We do this because all default
-- values should live in the dhall-configuration. The default invoc value is
-- simply no-change.
boolM :: ReadM Bool
boolM = maybeReader $ flip lookup [("on", True), ("off", False)]

invocP :: Parser Invoc
invocP = Invoc
  <$> runTypeP
  <*> cfgFileP
  <*> keymapFileP
  <*> fallthroughP
  <*> cmdAllowP
  <*> logLevelP
  <*> keyRepeatP
  <*> keyInputCfgP
  <*> keyOutputCfgP
  <*> preKIOcmdP
  <*> postKIOcmdP

runTypeP :: PM RunType
runTypeP = optional $ argument f ( help "What KMonad should do" )
  where f = maybeReader $ flip lookup
          [ ("run", FullRun), ("cfg-test", CfgTest), ("ev-test", EvTest) ]

cfgFileP :: PM FileSpec
cfgFileP = optional . strOption . mconcat $
  [ long "cfg-file"
  , short 'f'
  , metavar "FILE"
  , help "file-spec describing how to load the Dhall config file"
  ]

keymapFileP :: PM FileSpec
keymapFileP = optional . strOption . mconcat $
  [ long "kbd-file"
  , short 'k'
  , metavar "FILE"
  , help "file-spec describing how to load the keymap file"
  ]

fallthroughP :: PM Bool
fallthroughP = optional . option boolM . mconcat $
  [ long "reemit"
  , short 'r'
  , metavar "TOGGLE"
  , help "whether to reemit unhandled events"
  ]

cmdAllowP :: PM Bool
cmdAllowP = optional . option boolM . mconcat $
  [ long "commands"
  , short 'c'
  , metavar "TOGGLE"
  , help "whether to allow KMonad to execute shell commands"
  ]

logLevelP :: PM LogLevelSpec
logLevelP = optional . strOption . mconcat $
  [ long "log-level"
  , short 'l'
  , metavar "LEVEL"
  , help "at what level to display log messages"
  ]

keyRepeatP :: PM KeyRepeatSpec
keyRepeatP = optional . strOption . mconcat $
  [ long "key-repeat"
  , short 'e'
  , metavar "REPEAT"
  , help "repeat-spec describing how to perform key-repeat"
  ]

keyInputCfgP :: PM KeyInputSpec
keyInputCfgP = optional . strOption . mconcat $
  [ long "key-input"
  , short 'i'
  , metavar "INPUT"
  , help "input-spec describing how to capture keyboard input"]

keyOutputCfgP :: PM KeyOutputSpec
keyOutputCfgP = optional . strOption . mconcat $
  [ long "key-output"
  , short 'o'
  , metavar "OUTPUT"
  , help "output-spec describing how to inject keyboard output"
  ]

preKIOcmdP :: PM CmdSpec
preKIOcmdP = optional . strOption . mconcat $
  [ long "before-io-cmd"
  , short 'b'
  , metavar "CMD"
  , help "cmd-spec describing a shell-command to perform before initializing keyIO"
  ]

postKIOcmdP :: PM CmdSpec
postKIOcmdP = optional . strOption . mconcat $
  [ long "after-io-cmd"
  , short 'a'
  , metavar "CMD"
  , help "cmd-spec describing a shell-command to perform after initializing keyIO"]





-- fileP :: Parser FilePath
-- fileP = strArgument
--   (  metavar "FILE"
--   <> help    "The configuration file")

--     f = maybeReader $ flip lookup [ ("debug", LevelDebug), ("warn", LevelWarn)
--                                   , ("info",  LevelInfo),  ("error", LevelError) ]

-- | Parse the full command
-- cmdP :: Parser Cmd
-- cmdP =
--   Cmd <$> fileP
--       <*> dryrunP
--       <*> levelP
--       <*> startDelayP
--       <*> cmdAllowP
--       <*> fallThrghP
--       <*> initStrP
--       <*> cmpSeqP
--       <*> oTokenP
--       <*> iTokenP

-- -- | Parse a filename that points us at the config-file
-- fileP :: Parser FilePath
-- fileP = strArgument
--   (  metavar "FILE"
--   <> help    "The configuration file")

-- -- | Parse a flag that allows us to switch to parse-only mode
-- dryrunP :: Parser Bool
-- dryrunP = switch
--   (  long    "dry-run"
--   <> short   'd'
--   <> help    "If used, do not start KMonad, only try parsing the config file"
--   )


-- -- | Parse the log-level as either a level option or a verbose flag
-- levelP :: Parser LogLevel
-- levelP = option f
--   (  long    "log-level"
--   <> short   'l'
--   <> metavar "Log level"
--   <> value   LevelWarn
--   <> help    "How much info to print out (debug, info, warn, error)" )
--   where
--     f = maybeReader $ flip lookup [ ("debug", LevelDebug), ("warn", LevelWarn)
--                                   , ("info",  LevelInfo),  ("error", LevelError) ]

-- -- | Allow the execution of arbitrary shell-commands
-- cmdAllowP :: Parser DefSetting
-- cmdAllowP = SAllowCmd <$> switch
--   (  long "allow-cmd"
--   <> short 'c'
--   <> help "Whether to allow the execution of arbitrary shell-commands"
--   )

-- -- | Re-emit unhandled events
-- fallThrghP :: Parser DefSetting
-- fallThrghP = SFallThrough <$> switch
--   (  long "fallthrough"
--   <> short 'f'
--   <> help "Whether to simply re-emit unhandled events"
--   )

-- -- | TODO what does this do?
-- initStrP :: Parser (Maybe DefSetting)
-- initStrP = optional $ SInitStr <$> strOption
--   (  long "init"
--   <> short 't'
--   <> metavar "STRING"
--   <> help "TODO"
--   )

-- -- | Key to use for compose-key sequences
-- cmpSeqP :: Parser (Maybe DefSetting)
-- cmpSeqP = optional $ SCmpSeq <$> option
--   (tokenParser keywordButtons <|> megaReadM (M.choice noKeywordButtons))
--   (  long "cmp-seq"
--   <> short 's'
--   <> metavar "BUTTON"
--   <> help "Which key to use to emit compose-key sequences"
--   )

-- -- | Where to emit the output
-- oTokenP :: Parser (Maybe DefSetting)
-- oTokenP = optional $ SOToken <$> option (tokenParser otokens)
--   (  long "output"
--   <> short 'o'
--   <> metavar "OTOKEN"
--   <> help "Emit output to OTOKEN"
--   )

-- -- | How to capture the keyboard input
-- iTokenP :: Parser (Maybe DefSetting)
-- iTokenP = optional $ SIToken <$> option (tokenParser itokens)
--   (  long "input"
--   <> short 'i'
--   <> metavar "ITOKEN"
--   <> help "Capture input via ITOKEN"
--   )

-- -- | Parse a flag that disables auto-releasing the release of enter
-- startDelayP :: Parser Milliseconds
-- startDelayP = option (fromIntegral <$> megaReadM numP)
--   (  long  "start-delay"
--   <> short 'w'
--   <> value 300
--   <> showDefaultWith (show . unMS )
--   <> help  "How many ms to wait before grabbing the input keyboard (time to release enter if launching from terminal)")

-- -- | Transform a bunch of tokens of the form @(Keyword, Parser)@ into an
-- -- optparse-applicative parser
-- tokenParser :: [(Text, M.Parser a)] -> ReadM a
-- tokenParser = megaReadM . M.choice . map (M.try . uncurry ((*>) . symbol))

-- -- | Megaparsec <--> optparse-applicative interface
-- megaReadM :: M.Parser a -> ReadM a
-- megaReadM p = eitherReader (mapLeft show . M.parse p "" . fromString)
