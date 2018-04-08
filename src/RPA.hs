{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RPA where

import           Protolude hiding (hush)

import qualified Codec.Compression.Zlib as Zlib
import           Control.Monad.Trans.Maybe
import           Data.Bits (xor)
import qualified Data.ByteString as Bytes
import           Data.List (tail)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Language.Python.Pickle
import           Numeric (readHex)
import           Pipes
import qualified Pipes.ByteString as PB
import           System.Directory
import           System.FilePath
import           System.IO

data Header = Header
  { headerOffset :: Int
  , headerKey :: Int
  } deriving (Eq, Show)

data Entry = Entry
  { entryOffset :: Int
  , entryLength :: Int
  } deriving (Eq, Show)

newtype Index =
  Index (Map FilePath Entry)
  deriving (Eq, Show)


parseHeader :: ByteString -> Maybe Header
parseHeader s =
  let rawOffset = Bytes.take 16 . Bytes.drop 8 $ s
      rawKey = Bytes.take 8 . Bytes.drop 25 $ s
   in Header <$> decode rawOffset <*> decode rawKey
  where
    decode = fmap fst . head . readHex . toS

parseIndex :: Int -> Value -> Maybe Index
parseIndex key (PyDict d) = do
  entries <- traverse (parseEntry key) . Map.elems $ d
  keys <- traverse parseKey . Map.keys $ d
  return . Index . Map.fromList . zip keys $ entries
parseIndex _ _ = Nothing

parseEntry :: Int -> Value  -> Maybe Entry
parseEntry key (PyList [PyTuple [PyLong offset, PyLong len, PyString _]]) =
  Just (Entry (fromIntegral offset `xor` key) (fromIntegral len `xor` key))
parseEntry key (PyList [PyTuple [PyLong offset, PyInt len, PyString _]]) =
  Just (Entry (fromIntegral offset `xor` key) (fromIntegral len `xor` key))
parseEntry key (PyList [PyTuple [PyInt offset, PyLong len, PyString _]]) =
  Just (Entry (fromIntegral offset `xor` key) (fromIntegral len `xor` key))
parseEntry _ _ = Nothing

parseKey :: Value -> Maybe FilePath
parseKey (PyUnicode key) = Just . toS $ key
parseKey _ = Nothing

mkdirp :: FilePath -> IO ()
mkdirp path = do
  let paths = tail . map mconcat . inits . splitPath $ path
  for_ paths $ \subpath -> do
    doesExist <- doesDirectoryExist subpath
    unless doesExist (createDirectory subpath)

hush :: Either e a -> Maybe a
hush (Right x) = Just x
hush (Left _) = Nothing

extract :: FilePath -> IO ()
extract file = do
  mIndex <-
    withFile file ReadMode $ \h ->
      runMaybeT $ do
        headerBytes <- liftIO $ Bytes.hGetLine h
        header <- MaybeT . return . parseHeader $ headerBytes
        liftIO $ hSeek h AbsoluteSeek (fromIntegral . headerOffset $ header)
        rawIndex <- liftIO $ Zlib.decompress . toS <$> Bytes.hGetContents h
        unpickled <- MaybeT . return . hush . unpickle . toS $ rawIndex
        MaybeT . return . parseIndex (headerKey header) $ unpickled
  case mIndex of
    Nothing -> do
      putText "Failed to parse the archive index!"
      exitFailure
    Just (Index index)
      -- annoyingly hGetContents above closes the handle so we need to re-open
      -- it
     ->
      withFile file ReadMode $ \h ->
        for_ (Map.toList index) $ \(path, entry) -> do
          putText $
            "Extracting " <> toS path <> " (" <> show (entryLength entry) <>
            " bytes).."
          let dir = takeDirectory path
          mkdirp dir
          withFile path WriteMode $ \hh -> do
            hSeek h AbsoluteSeek (fromIntegral . entryOffset $ entry)
            runEffect $
              PB.fromHandle h >-> PB.take (entryLength entry) >-> PB.toHandle hh
