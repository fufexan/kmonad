-- |

module KMonad.App.Cfg.Ops where

import KMonad.Prelude
import KMonad.App.TypesNew
import KMonad.App.Cfg.Types
import KMonad.App.Cfg.Spec

import KMonad.Parsing
import KMonad.Gesture
-- import KMonad.Keyboard


import Control.Monad.Except
import Control.Exception.Lens

import qualified RIO.HashMap as M
import qualified RIO.Text as T
import qualified RIO.List as L
import qualified Dhall as D

loadAppCfg :: MonadIO m => Invoc -> m AppCfg
loadAppCfg = undefined

-- | Try to load a 'DhallCfg' from disk
loadDhallCfg :: MonadIO m => Maybe FileSpec -> m DhallCfg
loadDhallCfg mf = do
  pth <- getPath =<< case decode _FileSpec (fromMaybe defCfgFile mf) of
    Left e -> throwIO e
    Right x -> pure x
  let opt = D.defaultInterpretOptions { D.fieldModifier = T.drop 2 }
  let dec = D.genericAutoWith opt
  liftIO $ D.inputFile dec pth

--------------------------------------------------------------------------------
type Keycode = Natural


data LocaleError
  = DuplicateCodeNames [Name]
  | DuplicateGestureNames [Name]
  | OverlappingNames [Name]
  | LocaleParseError ParseError
  | LocaleGestureError [GestureReadError]
  | MissingKeyname [Name]
  deriving (Eq, Show)
makeClassyPrisms ''LocaleError
instance Exception LocaleError
instance AsLocaleError SomeException where _LocaleError = exception


-- | Try to translate the contents of a traversable, on error return failing key
--
-- This is a very general way of writing code that goes into a Gesture and
-- performs the (Name -> Keycode) mapping.
--
-- A more concrete way of writing this signature would be:
--
-- coded :: (Name -> Maybe Keycode) -> Gesture Name -> Either Name (Gesture Keycode)
--
coded :: Traversable t => (b -> Maybe a) -> t b -> Either b (t a)
coded f x = sequence $ (\n -> maybe (Left n) Right (f n)) <$> x

-- | Contruct a valid LocaleCfg or return an error detailing an issue.
mkLocale :: [(Name, Keycode)] -> [(Name, GestureExpr)] -> Either LocaleError LocaleCfg
mkLocale cl gl = do
  -- Check for duplicates and overlapping names
  let errAny p x = case x of
        [] -> Right ()
        ds -> Left $ p # ds
  errAny _DuplicateCodeNames    . duplicates $ cl^..folded._1
  errAny _DuplicateGestureNames . duplicates $ gl^..folded._1
  errAny _OverlappingNames . duplicates $ (cl^..folded._1) <> (gl^..folded._1)

  -- Create the keycode map (required for gesture finalization)
  let codes = M.fromList cl

  -- Check for valid gesture structure
  let (e1, gtxt) = partitionEithers . map readGesture $ gl^..folded._2
  errAny _LocaleGestureError e1

  -- Check that gestures only refer to existing keynames
  let (e2, gcode) = partitionEithers . over mapped (coded (`M.lookup` codes)) $ gtxt
  errAny _MissingKeyname e2

  -- Put it all together
  Right . LocaleCfg codes . M.fromList . zip (gl^..folded._1) $ gcode

data CfgError
  = CfgLocaleError LocaleError

-- | Take an 'Invoc' and a 'DhallCfg' and construct the 'AppCfg'
--
-- First, we take the dhall config (KMonad defaults are encoded in the dhall
-- configuration code) and we construct a valid AppCfg. Then we override any
-- settings provided in the invoc.
buildCfg :: Invoc -> DhallCfg -> Either CfgError AppCfg
buildCfg i d = do
  -- Validate and construct the locale
  loc <- left CfgLocaleError $
    mkLocale (d^.dcodeNames.from _DMap) (d^.dgestureNames.from _DMap)



  Right $ undefined


foo :: [Either LocaleError LocaleCfg]
foo = map (uncurry mkLocale)
  [ ([("a", 1), ("b", 2), ("S", 3)], [("party", "a b a b" ), ("joy", "S-[a b]")])
  , ([("a", 1), ("a", 2)], [])
  , ([], [("a", "b"), ("a", "c")])
  , ([("a", 1)], [("a", "l")])
  ]

fooM :: IO ()
fooM = pPrint foo
