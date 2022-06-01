let CodeNames = List { mapKey : Text, mapValue : Natural }

let GestureNames = List { mapKey : Text, mapValue : Text }

let KCfg =
      { codeNames : CodeNames
      , gestureNames : GestureNames
      , logLevel : Text
      , keyRepeat : Text
      , fallthrough : Bool
      , keyInputCfg : Text
      , keyOutputCfg : Text
      , preKIOcmd : Text
      , postKIOcmd : Text
      , keymapFile : Text
      , cmdAllow : Bool
      }

in  { KCfg, CodeNames, GestureNames }
