-- |

module KMonad.App.Cfg.Spec where

import KMonad.Prelude

import KMonad.App.TypesNew
import KMonad.App.Cfg.Types
import KMonad.Keyboard
import KMonad.Parsing

import qualified RIO.Text as T

-- basic -----------------------------------------------------------------------

-- file ------------------------------------------------------------------------

-- | A parser that tries to extract a 'File' from 'Text'
--
-- Patterns:
-- - starts with "cfg:" is read as XdgCfgFile
-- - starts with "glob:" is read as Glob
-- - starts with "/" is read as absolute path
fileP :: Parser File
fileP = choice
  [ string "cfg:" *> (XdgCfgFile <$> takeRest)
  , string "glob:" *> (Glob <$> takeRest)
  , lookAhead (char '/') *> (FullPath . unpack <$> takeRest)
  ]

-- | Create the textual representation of a 'File'
fileS :: File -> FileSpec
fileS (FullPath p) = pack p
fileS (XdgCfgFile t) = "cfg:" <> t
fileS (Glob t) = "glob:" <> t

-- | A Spec between 'Text' and 'FileSpec'
_FileSpec :: Spec File
_FileSpec = mkSpec fileS fileP

-- cmd -------------------------------------------------------------------------

-- | A parser that tries to extract a 'Cmd' from 'Text'
--
-- Patterns
-- - "cmd:evtest this thing for me"
-- - "exec:rm:["/*", "-rf"]"
cmdP :: Parser Cmd
cmdP = choice
  [ string "cmd:" *> (SimpleCmd <$> takeRest)
  , string "exec:" *> (CompoundCmd <$> exe <*> args)
  ]
  where
    exe = unpack <$> takeWhile1P Nothing (/= ':') <* char ':'
    args = listOfP textP

-- | Create the textual representation of a 'Cmd'
cmdS :: Cmd -> CmdSpec
cmdS (SimpleCmd t) = "cmd:" <> t
cmdS (CompoundCmd e a) = "exec:" <> pack e <> ":" <> tshow a

-- | A Spec between 'Cmd' and 'CmdSpec'
_CmdSpec :: Spec Cmd
_CmdSpec = mkSpec cmdS cmdP

-- input  ----------------------------------------------------------------------

-- | Return 'Nothing' if at 'eof', otherwise 'Just' the rest of the text
mayText :: Parser (Maybe Text)
mayText = Nothing <$ eof <|> Just <$> takeRest

-- | A parser that tries to extract an 'InputCfg' from 'Text'
--
-- Patterns
-- - lin:evdev:/dev/input8
-- - lin:evdev:glob:/dev/input/by-id/*das*event-kbd
-- - win:hook:
-- - mac:iokit:kb-name
-- - mac:iokit:
-- - cmdsrc:cmd:evtest this thing for me
-- - cmdsrc:exec:evtest:["this", "thing", "for", "me"]
-- - stdin:
inputP :: Parser KeyInputCfg
inputP = choice
  [ string "lin:evdev:" *> (LinEvdevSrc <$> fileP)
  , string "win:hook:" $> WinHookSrc
  , string "mac:iokit:" *> (MacIOKitSrc <$> mayText)
  , string "cmdsrc:" *> (CmdSrc <$> cmdP)
  , string "stdin:" $> StdinSrc
  ]

-- | Create the textual representation of an 'KeyInputCfg'
inputS :: KeyInputCfg -> KeyInputSpec
inputS (LinEvdevSrc f) = "lin:evdev:" <> fileS f
inputS WinHookSrc = "win:hook:"
inputS (MacIOKitSrc mt) = "mac:iokit:" <> fromMaybe "" mt
inputS (CmdSrc c) = "cmdsrc:" <> cmdS c
inputS StdinSrc = "stdin:"

-- | A Spec between 'KeyInputCfg' and 'KeyInputSpec'
_KeyInputSpec :: Spec KeyInputCfg
_KeyInputSpec = mkSpec inputS inputP

-- output ----------------------------------------------------------------------

-- | A parser that tries to extract an 'OutputCfg' from 'Text'
--
-- Patterns
-- - lin:uinput:
-- - lin:uinput:My keyboard name
-- - win:send:
-- - mac:kext:
-- - cmdsnk:cmd:evtest this thing for me
-- - cmdsnk:exec:evtest:["this", "thing", "for", "me"]
-- - stdout:
outputP :: Parser KeyOutputCfg
outputP = choice
  [ string "lin:uinput:" *> (LinUinputSnk <$> mayText)
  , string "win:send:" $> WinSendSnk
  , string "mac:kext:" $> MacKextSink
  , string "cmdsnk:" *> (CmdSnk <$> cmdP)
  , string "stdout:" $> StdoutSnk
  ]

-- | Create the textual representation of a 'KeyOutputCfg'
outputS :: KeyOutputCfg -> KeyOutputSpec
outputS (LinUinputSnk mt) = "lin:uinput:" <> fromMaybe "" mt
outputS WinSendSnk = "win:send:"
outputS MacKextSink = "mac:kext:"
outputS (CmdSnk c) = "cmdsnk:" <> cmdS c
outputS StdoutSnk = "stdout:"

-- | A Spec between 'KeyInputCfg' and 'KeyInputSpec'
_KeyOutputSpec :: Spec KeyOutputCfg
_KeyOutputSpec = mkSpec outputS outputP

-- key-repeat ------------------------------------------------------------------

-- | A parser that tries to extract a 'KeyRepeatCfg' from 'Text'
--
-- Patterns
-- - sim:300:100
keyRepeatP :: Parser KeyRepeatCfg
keyRepeatP = choice
  [ string "sim:" *> (Simulate <$> (DelayRate <$> (msP <* char ':') <*> msP))
  , string "echo:" $> EchoOS
  ]

-- | Create the textual representation of a 'KeyRepeatCfg'
keyRepeatS :: KeyRepeatCfg -> KeyRepeatSpec
keyRepeatS (Simulate (DelayRate d r)) = "sim:" <> tshow (d^.ms) <> ":" <> tshow (r^.ms)
keyRepeatS EchoOS = "echo:"

-- | A Spec between 'KeyRepeatCfg' and 'KeyRepeatSpec'
_KeyRepeatSpec :: Spec KeyRepeatCfg
_KeyRepeatSpec = mkSpec keyRepeatS keyRepeatP

-- log -------------------------------------------------------------------------

-- | Try to parse a 'LogLevel' from 'Text'
logLevelP :: Parser LogLevel
logLevelP = choice
  [ string "debug" $> LevelDebug
  , string "info"  $> LevelInfo
  , string "warn"  $> LevelWarn
  , string "error" $> LevelError
  ]

-- | Create the textual representatino of a 'LogLevel'
logLevelS :: LogLevel -> LogLevelSpec
logLevelS = T.drop 5 . T.toLower . tshow

-- | A Spec between 'LogLevel' and 'LogLevelSpec'
_LogLevelSpec :: Spec LogLevel
_LogLevelSpec = mkSpec logLevelS logLevelP
