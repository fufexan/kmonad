let T = ./types.dhall

let L = ./locale/linux/enUS.dhall

let base =
      { logLevel = "warn"
      , keymapFile = "cfg:keymap.kbd"
      , cmdAllow = False
      , fallthrough = False
      , keyRepeat = None Text
      , preKIOcmd = None Text
      , postKIOcmd = None Text
      }

let linux = base //
      { codeNames = L.numbers # (toMap L.codenames) : T.CodeNames
      , gestureNames = [] : T.GestureNames
      , keyInputCfg = "evdev:glob:/dev/input/by-id/*kbd"
      , keyOutputCfg = "uinput:KMonad"
      } : T.KCfg

let windows = base //
      { codeNames = L.numbers # (toMap L.codenames) : T.CodeNames
      , gestureNames = [] : T.GestureNames
      , keyInputCfg = "hook:"
      , keyOutputCfg = "send:"
      } : T.KCfg

in { linux, windows }
