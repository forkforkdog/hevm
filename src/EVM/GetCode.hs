module EVM.GetCode
  ( GetCodeEnvPreference(..)
  , defaultMaxArtifactBytes
  , getCodeFromEnv
  , resolveGetCode
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Data.Aeson (Value(..), decodeStrict')
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as BS16
import Data.Char (isDigit, toLower)
import Data.List (isPrefixOf, isSuffixOf, sort, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import EVM.Solidity (containsLinkerHole)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, getCurrentDirectory, getFileSize, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (addTrailingPathSeparator, isAbsolute, takeBaseName, takeDirectory, takeFileName, (</>))
import Text.Read (readMaybe)

data GetCodeEnvPreference
  = PreferHevm
  | PreferEchidna
  deriving (Eq, Show)

data Selector
  = SelDirectPath FilePath
  | SelFileContract String String
  | SelFileVersion String String
  | SelContractVersion String String
  | SelFileOnly String
  | SelContractOnly String
  deriving (Eq, Show)

data ArtifactRecord = ArtifactRecord
  { artifactPath :: FilePath
  , sourcePath :: Maybe String
  , sourceFileName :: Maybe String
  , contractName :: Maybe String
  , versionNorm :: Maybe String
  , bytecodeResult :: Either String BS.ByteString
  }
  deriving (Eq, Show)

defaultMaxArtifactBytes :: Integer
defaultMaxArtifactBytes = 5 * 1024 * 1024

getCodeFromEnv :: GetCodeEnvPreference -> FilePath -> IO (Either String BS.ByteString)
getCodeFromEnv pref ref = do
  root <- getArtifactsRoot pref
  maxBytes <- getArtifactsMaxBytes pref
  prefVersion <- getPreferredVersion pref
  resolveGetCode root maxBytes prefVersion ref

resolveGetCode :: FilePath -> Integer -> Maybe String -> FilePath -> IO (Either String BS.ByteString)
resolveGetCode root maxBytes prefVersion ref = do
  case parseSelector ref of
    Left err -> pure (Left err)
    Right (SelDirectPath p) ->
      resolveDirectPath root maxBytes p
    Right selector -> do
      records <- buildArtifactIndex root maxBytes
      pure $ do
        rec <- selectRecord selector prefVersion records
        rec.bytecodeResult

resolveDirectPath :: FilePath -> Integer -> FilePath -> IO (Either String BS.ByteString)
resolveDirectPath root maxBytes ref = do
  eBs <- safeReadFileUnderRoot root maxBytes ref
  pure $ do
    bs <- eBs
    value <- first (("getCode: invalid artifact JSON: " <>) . show) (decodeArtifactJson bs)
    extractBytecode value

parseSelector :: FilePath -> Either String Selector
parseSelector ref
  | isJsonPath ref = Right (SelDirectPath ref)
  | otherwise =
      case splitOn ":" ref of
        [one] ->
          if isSolFileRef one
            then Right (SelFileOnly one)
            else Right (SelContractOnly one)
        [left, right]
          | null left || null right ->
              Left "getCode: malformed artifact reference"
          | isSolFileRef left ->
              if isVersionSelector right
                then Right (SelFileVersion left right)
                else Right (SelFileContract left right)
          | isVersionSelector right ->
              Right (SelContractVersion left right)
          | otherwise ->
              Left "getCode: malformed artifact reference"
        _ ->
          Left "getCode: malformed artifact reference"

isJsonPath :: FilePath -> Bool
isJsonPath p = ".json" `isSuffixOf` fmap toLower p

isSolFileRef :: String -> Bool
isSolFileRef p = ".sol" `isSuffixOf` fmap toLower p

isVersionSelector :: String -> Bool
isVersionSelector s = case normalizeVersion s of
  Nothing -> False
  Just v -> length (filter (== '.') v) >= 2

decodeArtifactJson :: BS.ByteString -> Either String Value
decodeArtifactJson bs = case decodeStrict' bs of
  Just v -> Right v
  Nothing -> Left "unable to decode JSON"

decodeArtifactRecord :: FilePath -> BS.ByteString -> Maybe ArtifactRecord
decodeArtifactRecord path bs = do
  value <- decodeStrict' bs
  let src = extractSourcePath path value
      srcFile = takeFileName <$> src
      cName = extractContractName path value
      vNorm = extractVersion value >>= normalizeVersion
      codeRes = extractBytecode value
  pure ArtifactRecord
    { artifactPath = path
    , sourcePath = src
    , sourceFileName = srcFile
    , contractName = cName
    , versionNorm = vNorm
    , bytecodeResult = codeRes
    }

extractBytecode :: Value -> Either String BS.ByteString
extractBytecode value = do
  hexTxt <- maybeToEither "getCode: missing field bytecode.object" $
    firstJust
      [ lookupTextPath ["bytecode", "object"] value
      , lookupTextPath ["evm", "bytecode", "object"] value
      , lookupTextPath ["bytecode"] value
      ]
  let raw = T.unpack (T.strip hexTxt)
  if containsLinkerHole (T.pack raw)
    then Left "getCode: unlinked libraries in bytecode"
    else do
      let stripped = strip0x raw
      if null stripped
        then Right BS.empty
        else first (const "getCode: invalid hex bytecode") $
          BS16.decodeBase16Untyped (encodeUtf8 (T.pack stripped))

extractContractName :: FilePath -> Value -> Maybe String
extractContractName path value =
  fmap T.unpack (lookupTextPath ["contractName"] value) <|> Just (takeBaseName path)

extractSourcePath :: FilePath -> Value -> Maybe String
extractSourcePath path value =
  fmap T.unpack (lookupTextPath ["sourceName"] value)
  <|> fmap T.unpack (lookupTextPath ["ast", "absolutePath"] value)
  <|> parentSol
  where
    parent = takeFileName (takeDirectory path)
    parentSol = if isSolFileRef parent then Just parent else Nothing

extractVersion :: Value -> Maybe String
extractVersion value =
  fmap T.unpack (lookupTextPath ["compiler", "version"] value)
  <|> metadataVersion
  where
    metadataVersion =
      case lookupValuePath ["metadata"] value of
        Just (String t) -> do
          meta <- decodeStrict' (encodeUtf8 t)
          fmap T.unpack (lookupTextPath ["compiler", "version"] meta)
        Just obj@(Object _) ->
          fmap T.unpack (lookupTextPath ["compiler", "version"] obj)
        _ -> Nothing

lookupValuePath :: [Text] -> Value -> Maybe Value
lookupValuePath [] v = Just v
lookupValuePath (k:ks) (Object obj) =
  KM.lookup (Key.fromText k) obj >>= lookupValuePath ks
lookupValuePath _ _ = Nothing

lookupTextPath :: [Text] -> Value -> Maybe Text
lookupTextPath ks v = case lookupValuePath ks v of
  Just (String t) -> Just t
  _ -> Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust = \case
  [] -> Nothing
  (x:xs) -> case x of
    Just _ -> x
    Nothing -> firstJust xs

normalizeVersion :: String -> Maybe String
normalizeVersion raw =
  let trimmed = dropWhile (== ' ') raw
      noV = case trimmed of
        ('v':xs) -> xs
        ('V':xs) -> xs
        xs -> xs
      prefix = takeWhile (\c -> isDigit c || c == '.') noV
  in if length (filter (== '.') prefix) >= 2
       then Just prefix
       else Nothing

matchesVersion :: String -> Maybe String -> Bool
matchesVersion selector recV =
  case (normalizeVersion selector, recV) of
    (Just sv, Just rv) -> sv == rv
    _ -> False

selectRecord :: Selector -> Maybe String -> [ArtifactRecord] -> Either String ArtifactRecord
selectRecord selector prefVersion records = do
  let sorted = sortOn (.artifactPath) records
  case selector of
    SelFileContract f c ->
      pickWithoutVersion prefVersion $
        filter (\r -> matchesFile f r && matchesContract c r) sorted
    SelFileVersion f v -> do
      let base = filter (matchesFile f) sorted
      if null base
        then Left "getCode: artifact not found"
        else pickWithExplicitVersion v $
          filter (\r -> matchesVersion v r.versionNorm) base
    SelContractVersion c v -> do
      let base = filter (matchesContract c) sorted
      if null base
        then Left "getCode: artifact not found"
        else pickWithExplicitVersion v $
          filter (\r -> matchesVersion v r.versionNorm) base
    SelFileOnly f -> do
      let base = filter (matchesFile f) sorted
          fileBaseName = takeBaseName (takeFileName f)
          sameName = filter (\r -> matchesContract fileBaseName r) base
      if null base
        then Left "getCode: artifact not found"
        else if null sameName
          then pickWithoutVersion prefVersion base
          else pickWithoutVersion prefVersion sameName
    SelContractOnly c ->
      pickWithoutVersion prefVersion $
        filter (matchesContract c) sorted
    SelDirectPath _ ->
      Left "getCode: malformed artifact reference"
  where
    pickWithExplicitVersion :: String -> [ArtifactRecord] -> Either String ArtifactRecord
    pickWithExplicitVersion _ candidates =
      case candidates of
        [] -> Left "getCode: compiler version not found"
        [r] -> Right r
        _ -> Left "getCode: ambiguous artifact reference"

pickWithoutVersion :: Maybe String -> [ArtifactRecord] -> Either String ArtifactRecord
pickWithoutVersion prefVersion candidates =
  case candidates of
    [] -> Left "getCode: artifact not found"
    [r] -> Right r
    xs -> case prefVersion of
      Just pref ->
        case filter (\r -> matchesVersion pref r.versionNorm) xs of
          [r] -> Right r
          [] -> Left "getCode: ambiguous artifact reference"
          _ -> Left "getCode: ambiguous artifact reference"
      Nothing -> Left "getCode: ambiguous artifact reference"

matchesFile :: String -> ArtifactRecord -> Bool
matchesFile selector rec =
  let wanted = normalizePath selector
      wantedBase = takeFileName wanted
      sourceFull = normalizePath <$> rec.sourcePath
      sourceBase = fmap normalizePath rec.sourceFileName
      matchesFull = maybe False (\sf -> sf == wanted || ("/" <> wanted) `isSuffixOf` sf) sourceFull
      matchesBase = maybe False (\sb -> sb == wantedBase) sourceBase
  in matchesFull || matchesBase

matchesContract :: String -> ArtifactRecord -> Bool
matchesContract selector rec = rec.contractName == Just selector

normalizePath :: FilePath -> FilePath
normalizePath = fmap (\c -> if c == '\\' then '/' else c)

strip0x :: String -> String
strip0x ('0':'x':xs) = xs
strip0x ('0':'X':xs) = xs
strip0x xs = xs

buildArtifactIndex :: FilePath -> Integer -> IO [ArtifactRecord]
buildArtifactIndex root maxBytes = do
  files <- listJsonFilesRecursive root
  catMaybes <$> mapM decodeOne files
  where
    decodeOne fp = do
      eBs <- safeReadFileUnderRoot root maxBytes fp
      pure $ do
        bs <- either (const Nothing) Just eBs
        decodeArtifactRecord fp bs

listJsonFilesRecursive :: FilePath -> IO [FilePath]
listJsonFilesRecursive root = do
  exists <- doesDirectoryExist root
  if not exists then pure [] else do
    rootAbs <- canonicalizePath root
    go rootAbs
  where
    skipDirs = ["build-info", "kompiled", ".git", "dist-newstyle", "result"]
    go dir = do
      entries <- sort <$> listDirectory dir
      nested <- forM entries $ \entry -> do
        let p = dir </> entry
        isDir <- doesDirectoryExist p
        if isDir
          then if entry `elem` skipDirs
                 then pure []
                 else go p
          else pure $
            if isJsonPath p && not (".metadata.json" `isSuffixOf` fmap toLower p)
              then [p]
              else []
      pure (concat nested)

safeReadFileUnderRoot :: FilePath -> Integer -> FilePath -> IO (Either String BS.ByteString)
safeReadFileUnderRoot root maxBytes path = do
  eRootAbs <- (try (canonicalizePath root) :: IO (Either SomeException FilePath))
  case eRootAbs of
    Left e -> pure $ Left ("getCode: invalid artifacts root: " <> show e)
    Right rootAbs -> do
      let candidate = if isAbsolute path then path else rootAbs </> path
      eFileAbs <- (try (canonicalizePath candidate) :: IO (Either SomeException FilePath))
      case eFileAbs of
        Left e -> pure $ Left ("getCode: cannot canonicalize path: " <> show e)
        Right fileAbs -> do
          let rootPrefix = addTrailingPathSeparator rootAbs
              inRoot = fileAbs == rootAbs || rootPrefix `isPrefixOf` fileAbs
          if not inRoot then
            pure $ Left "getCode: path escapes artifacts root"
          else do
            exists <- doesFileExist fileAbs
            if not exists then
              pure $ Left "getCode: artifact not found"
            else do
              eSize <- (try (getFileSize fileAbs) :: IO (Either SomeException Integer))
              case eSize of
                Left e -> pure $ Left ("getCode: cannot stat artifact: " <> show e)
                Right sz ->
                  if sz > maxBytes then
                    pure $ Left ("getCode: artifact too large (" <> show sz <> " bytes)")
                  else do
                    eBs <- (try (BS.readFile fileAbs) :: IO (Either SomeException BS.ByteString))
                    pure $ case eBs of
                      Left e -> Left ("getCode: read failed: " <> show e)
                      Right bs -> Right bs

getArtifactsRoot :: GetCodeEnvPreference -> IO FilePath
getArtifactsRoot pref =
  lookupFirstEnv (rootEnvOrder pref) >>= \case
    Just r -> pure r
    Nothing -> getCurrentDirectory
  where
    rootEnvOrder PreferHevm =
      [ "HEVM_ARTIFACTS_ROOT"
      , "ECHIDNA_ARTIFACTS_ROOT"
      , "HEVM_FS_ROOT"
      , "ECHIDNA_FS_ROOT"
      ]
    rootEnvOrder PreferEchidna =
      [ "ECHIDNA_ARTIFACTS_ROOT"
      , "HEVM_ARTIFACTS_ROOT"
      , "ECHIDNA_FS_ROOT"
      , "HEVM_FS_ROOT"
      ]

getArtifactsMaxBytes :: GetCodeEnvPreference -> IO Integer
getArtifactsMaxBytes pref =
  lookupFirstPositiveInt (maxEnvOrder pref) >>= \case
    Just n -> pure n
    Nothing -> pure defaultMaxArtifactBytes
  where
    maxEnvOrder PreferHevm =
      [ "HEVM_ARTIFACTS_MAX_BYTES"
      , "ECHIDNA_ARTIFACTS_MAX_BYTES"
      , "HEVM_FS_MAX_BYTES"
      , "ECHIDNA_FS_MAX_BYTES"
      ]
    maxEnvOrder PreferEchidna =
      [ "ECHIDNA_ARTIFACTS_MAX_BYTES"
      , "HEVM_ARTIFACTS_MAX_BYTES"
      , "ECHIDNA_FS_MAX_BYTES"
      , "HEVM_FS_MAX_BYTES"
      ]

getPreferredVersion :: GetCodeEnvPreference -> IO (Maybe String)
getPreferredVersion pref =
  fmap (>>= normalizeVersion) (lookupFirstEnv order)
  where
    order = case pref of
      PreferHevm ->
        [ "HEVM_SOLC_VERSION"
        , "ECHIDNA_SOLC_VERSION"
        , "FOUNDRY_SOLC_VERSION"
        , "SOLC_VERSION"
        ]
      PreferEchidna ->
        [ "ECHIDNA_SOLC_VERSION"
        , "HEVM_SOLC_VERSION"
        , "FOUNDRY_SOLC_VERSION"
        , "SOLC_VERSION"
        ]

lookupFirstEnv :: [String] -> IO (Maybe String)
lookupFirstEnv = \case
  [] -> pure Nothing
  (k:ks) -> lookupEnv k >>= \case
    Just v | not (null v) -> pure (Just v)
    _ -> lookupFirstEnv ks

lookupFirstPositiveInt :: [String] -> IO (Maybe Integer)
lookupFirstPositiveInt = \case
  [] -> pure Nothing
  (k:ks) -> lookupEnv k >>= \case
    Just raw | Just n <- readMaybe raw, n > 0 -> pure (Just n)
    _ -> lookupFirstPositiveInt ks

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = \case
  Just a -> Right a
  Nothing -> Left e
