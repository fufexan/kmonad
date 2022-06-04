-- |

module KMonad.App.Cfg.Expr.File where

import KMonad.Prelude hiding (try)

import KMonad.Parsing

import System.FilePath.Glob (glob)
import System.Info (os)
import UnliftIO.Directory
import RIO.FilePath ((</>))

import qualified RIO      as S (unlines)
import qualified RIO.Text as T

-- basic types -----------------------------------------------------------------

-- | Root directories we know how to search
data PathRoot
  = XdgCfg      -- ^ The app-configuration directory plus "/kmonad"
  | Home        -- ^ Home directory
  | Custom Text -- ^ Any other path-prefix, may contain globs
  deriving (Eq, Show)

-- | How to look for a particular filepath
data Path = Path
  { _val  :: Text           -- ^ The pattern to match
  , _root :: Maybe PathRoot -- ^ Optionally a path to be relative to
  , _glb  :: Bool           -- ^ Whether to glob-match this expression
  } deriving (Eq, Show)
makeLenses ''Path

-- errors ----------------------------------------------------------------------

-- | Things that can go wrong with 'Path' resolution
data PathError
  = GlobNoMatch         FilePath
  | GlobMultipleMatches FilePath [FilePath]
  deriving Eq
makeClassyPrisms ''PathError

instance Show PathError where
  show (GlobNoMatch t) =
    "Glob \"" <> t <> "\" matches 0 files"
  show (GlobMultipleMatches t fs)  = S.unlines $ [
    "Glob \"" <> t <> "\" matches multiple files:"
    ] <> fs

instance Exception PathError
instance AsPathError SomeException where _PathError = exception

-- basic ops -------------------------------------------------------------------

-- | Resolve a 'Path' value to an absolute 'FilePath'
--
-- This may throw a
-- * GlobNoMatch when a glob matches no files
-- * GlobMultipleMatches when a glob matches more than 1 file
--
-- Note that a succesful resolution does not guarantee the file exists, only
-- that during the 'resolve' call the 'Path' made enough sense to create a
-- single 'FilePath'.
resolve :: MonadIO m => Path -> m FilePath
resolve p = do
  r <- (</> (unpack $ p^.val)) <$> case p^.root of
    Nothing         -> pure ""
    Just XdgCfg     -> getXdgDirectory XdgConfig "kmonad"
    Just Home       -> getHomeDirectory
    Just (Custom t) -> pure $ unpack t

  if not $ p^.glb then pure r else (liftIO . glob $ r) >>= \case
    [] -> throwIO . GlobNoMatch $ r
    [f] -> pure f
    fs  -> throwIO $ GlobMultipleMatches r fs

-- exprs -----------------------------------------------------------------------

{- NOTE:

"[glob:][root:]rest/of/the/expression.ext"
"[[g][c]:]rest/of/the/expression.ext"

shorthand:
g: glob
x: xdgcfg
h: home
-}

-- | Predefined terms referring to certain standard 'PathRoot's
roots :: Named PathRoot
roots = [("xdgcfg", XdgCfg), ("home", Home)]

-- | Create a 'Parser' for 'Path's, allowing for additional 'Named PathRoot's.
pathP :: Named PathRoot -> Parser Path
pathP nr = do

  -- Make sure there is no clash between names or characters
  let r = map (over _1 (<> ":")) roots <> nr -- All ("long-name", Root)s
  let l = validated $ "glob": (r^..names)    -- All long preamble terms, validated
  let s = validated $ map (T.take 1) l       -- All short preample terms, validated

  pre <- preambleLong r <|> preambleShort (over (mapped._1) (T.take 1) r)
  rst <- exprTail

  pure $ Path rst (pre^._2) (pre^._1)

-- | Parse a long preamble consisting of full, colon-terminated names
preambleLong  :: Named PathRoot -> Parser (Bool, Maybe PathRoot)
preambleLong nr = do
  g <- optional . try $ string "glob:"
  r <- optional . namedP $ nr
  case (g, r) of (Nothing, Nothing) -> empty
                 _                  -> pure (isJust g, r)

-- | Parse a short preamble consisting of initial characters followed by a colon
preambleShort :: Named PathRoot -> Parser (Bool, Maybe PathRoot)
preambleShort nr = do
  p <- optional . try $ do
    g' <- optional $ char 'g'
    r' <- optional . namedP $ nr
    (g', r') <$ char ':'
  pure . maybe (False, Nothing) (over _1 isJust) $ p

-- | Parse the non-preamble portion of the string, any non-colon or double-colon
exprTail :: Parser Text
exprTail = do
  let nc = takeWhile1P (Just "non-colon character") (/= ':')
  let ec = ":" <$ string "::" <?> ":: (escaped colon)"
  mconcat <$> some (try ec <|> nc) <* (void eol <|> eof)

-- | Try to read a 'Path' value from some text
readPath :: Text -> Either ParseError Path
readPath = parse (pathP [])
