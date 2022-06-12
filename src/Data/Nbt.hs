{-
Module      :  Data.Nbt
Copyright   :  (c) David R. Garland 2022
License     :  MIT
Maintainer  :  davidrgarland@me.com

Functions related to parsing & serializing the NBT file format.
-}

module Data.Nbt
  ( Type (..)
  , Nbt (..)
  , Nbt'
  , MapNbt
  , Tag (..)
  , Tag'
  , MapTag
  , Cmpnd (..)
  , Cmpnd'
  , MapCmpnd
  , pattern Compound'
  , lookupNbt
  , getNbt
  , lookupTag
  , getTag
  , typeOf
  , readUncompressed
  , writeUncompressed
  , readCompressed
  , writeCompressed
  ) where

import Codec.Compression.GZip as GZ
import Control.Monad
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable
import Data.Foldable.WithIndex
import Data.Int
import Data.Map.Strict qualified as M
import Data.Map.Strict (Map)
import Data.RRBVector qualified as R
import Data.RRBVector (Vector, (<|))
import Data.Serialize hiding (label)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Word

-- | An NBT tag type, in order so that 'toEnum' and 'fromEnum' are useful for serialization and
-- deserialization.
data Type
  = EndType       -- ^ 0x00 NUL. Denotes the end of a file.
  | ByteType      -- ^ 0x01 SOH. Corresponds to `Data.Nbt.Byte`.
  | ShortType     -- ^ 0x02 STX. Corresponds to `Data.Nbt.Short`.
  | IntType       -- ^ 0x03 ETX. Corresponds to `Data.Nbt.Int`.
  | LongType      -- ^ 0x04 EOT. Corresponds to `Data.Nbt.Long`.
  | FloatType     -- ^ 0x05 ENQ. Corresponds to `Data.Nbt.Float`.
  | DoubleType    -- ^ 0x06 ACK. Corresponds to `Data.Nbt.Double`.
  | ByteArrayType -- ^ 0x07 BEL. Corresponds to `ByteArray`.
  | StringType    -- ^ 0x08 BS. Corresponds to `Data.Nbt.String`.
  | ListType      -- ^ 0x09 HT. Corresponds to `List`.
  | CompoundType  -- ^ 0x0a LF. Corresponds to `Compound`.
  | IntArrayType  -- ^ 0x0b VT. Corresponds to `IntArray`.
  | LongArrayType -- ^ 0x0c FF. Corresponds to `LongArray`.
  deriving (Show, Eq, Ord, Enum)

-- | The main NBT type, with support for serialization and deserialization. It couples a 'Text' label with a 'Tag'.
-- 
-- When serialized: 1-byte unsigned 'Type' + 2-byte unsigned integer length + N bytes utf-8 text + 'Tag'
data Nbt b = Nbt
  { label :: Text
  , tag :: Tag b
  }
  deriving (Show, Eq, Ord, Functor)

-- | The "simple" form of `Nbt`, which has no label map on `Compound`s.
type Nbt' = Nbt ()

-- | A version of `Nbt` which has a map from labels to element indices for fast lookup.
type MapNbt = Nbt (Map Text Int)

-- | An NBT tag, responsible for storing the actual data.
data Tag b
  = Byte      Int8             -- ^ 1-byte signed integer
  | Short     Int16            -- ^ 2-byte signed integer
  | Int       Int32            -- ^ 4-byte signed integer
  | Long      Int64            -- ^ 8-byte signed integer
  | Float     Float            -- ^ 4-byte float
  | Double    Double           -- ^ 8-byte double
  | ByteArray (Vector Int8)    -- ^ 4-byte signed integer length + N 1-byte signed integers
  | String    Text             -- ^ 2-byte unsigned integer length + N bytes utf-8 text
  | List      (Vector (Tag b)) -- ^ 1-byte unsigned 'Type' + 4-byte signed integer length + N homogenous tags
  | Compound  (Cmpnd b)        -- ^ N heterogenous NBT elements punctuated by an 'EndType'
  | IntArray  (Vector Int32)   -- ^ 4-byte signed integer length + N 4-byte signed integers
  | LongArray (Vector Int64)   -- ^ 4-byte signed integer length + N 8-byte signed integers
  deriving (Show, Eq, Ord, Functor)

-- | A pattern synonym for "Compound (Cmpnd () v)".
pattern Compound' :: Vector Nbt' -> Tag'
pattern Compound' v <- Compound (Cmpnd () v)
  where Compound' v = Compound (Cmpnd () v)
{-# COMPLETE Byte, Short, Int, Long, Float, Double, ByteArray, String, List, Compound', IntArray, LongArray #-}

-- | The "simple" form of `Tag`, which has no label map on `Compound`s.
type Tag' = Tag ()

-- | A version of `Tag` which has a map from labels to element indices for fast lookup.
type MapTag = Tag (Map Text Int)

-- | The payload of the `Compound` constructor.
data Cmpnd b = Cmpnd b (Vector (Nbt b))
  deriving (Show, Eq, Ord, Functor)

-- | The "simple" form of `Cmpnd`, which has no label map.
type Cmpnd' = Cmpnd ()

-- | A version of `Cmpnd` which has a map from labels to element indices for fast lookup.
type MapCmpnd = Cmpnd (Map Text Int)

-- | Use a `MapCmpnd` to quickly look up a `MapNbt` by label.
lookupNbt :: Text -> MapCmpnd -> Maybe MapNbt
lookupNbt t (Cmpnd m v) = (v R.!) <$> M.lookup t m

-- | Use a `MapCmpnd` to quickly get a `MapNbt` by label.
getNbt :: Text -> MapCmpnd -> MapNbt
getNbt t (Cmpnd m v) = v R.! (m M.! t)

-- | Runs `lookupNbt`, then retrieves the `tag`.
lookupTag :: Text -> MapCmpnd -> Maybe MapTag
lookupTag t c = tag <$> lookupNbt t c

-- | Runs `getNbt`, then retrieves the `tag`.
getTag :: Text -> MapCmpnd -> MapTag
getTag t c = tag $ getNbt t c

instance Serialize (Nbt ()) where
  get = get >>= getNbtByType
  put (Nbt n d) = put (typeOf d) >> putString n >> putTag d

instance Serialize (Nbt (Map Text Int)) where
  get = get >>= getNbtByType
  put (Nbt n d) = put (typeOf d) >> putString n >> putTag d

instance Serialize Type where
  get = toEnum . fromIntegral <$> getWord8
  put = putWord8 . fromIntegral . fromEnum

class GenMap b where
  genMap :: Vector (Nbt b) -> b

instance GenMap () where
  genMap = const ()
  {-# INLINE CONLIKE genMap #-}

instance GenMap (Map Text Int) where
  genMap = ifoldr (\i (Nbt l _) a -> M.insert l i a) M.empty
  {-# INLINE genMap #-}

-- | Get the type of an NBT tag.
typeOf :: Tag b -> Type
typeOf = \case
  Byte _ -> ByteType
  Short _ -> ShortType
  Int _ -> IntType
  Long _ -> LongType
  Float _ -> FloatType
  Double _ -> DoubleType
  ByteArray _ -> ByteArrayType
  String _ -> StringType
  List _ -> ListType
  Compound _ -> CompoundType
  IntArray _ -> IntArrayType
  LongArray _ -> LongArrayType

putTag :: Serialize (Nbt b) => Tag b -> Put
putTag = \case
  Byte b -> put b
  Short s -> put s
  Int i -> put i
  Long l -> put l
  Float f -> putFloat32be f
  Double d -> putFloat64be d
  ByteArray bs -> putArray put bs
  String str -> putString str
  List ts -> do
    ty <-
      if null ts then do
        pure EndType
      else do
        let xt = typeOf (ts R.! 0)
        let xs = R.drop 1 ts
        if all (\e -> typeOf e == xt) xs then
          pure xt
        else
          error "attempted to write heterogenous list"
    put ty
    putArray putTag ts
  Compound (Cmpnd _ ts) -> traverse_ put ts >> put EndType
  IntArray is -> putArray put is
  LongArray is -> putArray put is
  where
    putArray :: (e -> Put) -> Vector e -> Put
    putArray putter a = do
      put (fromIntegral (length a) :: Int32)
      traverse_ putter a
{-# SPECIALIZE putTag :: Tag () -> Put #-}
{-# SPECIALIZE putTag :: Tag (Map Text Int) -> Put #-}

getNbtByType :: GenMap b => Type -> Get (Nbt b)
getNbtByType ty = Nbt <$> getString <*> getByType ty
{-# SPECIALIZE getNbtByType :: Type -> Get (Nbt ()) #-}
{-# SPECIALIZE getNbtByType :: Type -> Get (Nbt (Map Text Int)) #-}

getByType :: GenMap b => Type -> Get (Tag b)
getByType = \case
  EndType -> fail "cannot get a value of EndType"
  ByteType -> Byte <$> get
  ShortType -> Short <$> get
  IntType -> Int <$> get
  LongType -> Long <$> get
  FloatType -> Float <$> getFloat32be
  DoubleType -> Double <$> getFloat64be
  ByteArrayType -> ByteArray <$> getArray get
  StringType -> String <$> getString
  ListType -> List <$> (getArray . getByType =<< get)
  CompoundType -> do
    x <- getCompound
    pure $ Compound (Cmpnd (genMap x) x)
  IntArrayType -> IntArray <$> getArray get
  LongArrayType -> LongArray <$> getArray get
  where
    getCompound :: GenMap b => Get (Vector (Nbt b))
    getCompound = get >>= \case
      EndType -> pure R.empty
      ty -> (<|) <$> getNbtByType ty <*> getCompound
{-# SPECIALIZE getByType :: Type -> Get (Tag ()) #-}
{-# SPECIALIZE getByType :: Type -> Get (Tag (Map Text Int)) #-}

{-
-- File IO
-}

-- | Read an uncompressed NBT file. Exceptions are thrown as usual if the internal call to 'Data.ByteString.readFile' fails,
-- and the 'Either' message is that returned by 'Data.Serialize.decode'.
readUncompressed :: Serialize (Nbt b) => FilePath -> IO (Either String (Nbt b))
readUncompressed fp = decode <$> B.readFile fp

-- | Write an uncompressed NBT file.
writeUncompressed :: Serialize (Nbt b) => FilePath -> Nbt b -> IO ()
writeUncompressed fp nbt = B.writeFile fp $ runPut (put nbt)

-- | Read a GZip-compressed NBT file. Exceptions are thrown as usual if the internal call to 'Data.ByteString.Lazy.readFile' fails,
-- and the 'Either' message is that returned by 'Data.Serialize.decode'.
readCompressed :: Serialize (Nbt b) => FilePath -> IO (Either String (Nbt b))
readCompressed fp = do
  bs <- GZ.decompress <$> BL.readFile fp
  pure . decode $ B.toStrict bs

-- | Write a GZip-compressed NBT file.
writeCompressed :: Serialize (Nbt b) => FilePath -> Nbt b -> IO ()
writeCompressed fp nbt = do
  let bs = GZ.compress . BL.fromStrict $ runPut (put nbt)
  B.writeFile fp (B.toStrict bs)

{-
-- Internal Utility Functions
-}

getArray :: Get e -> Get (Vector e)
getArray getter = do
  len <- get :: Get Int32
  elts <- replicateM (fromIntegral len) getter
  pure . R.fromList $ elts

getString :: Get Text
getString = do
  len <- get :: Get Word16
  decodeUtf8 <$> getByteString (fromIntegral len)

putString :: Text -> Put
putString t = do
  let b = encodeUtf8 t
  put (fromIntegral (B.length b) :: Word16)
  putByteString b

