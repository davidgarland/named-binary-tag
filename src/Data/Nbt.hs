module Data.Nbt
  ( Type (..)
  , Nbt (..)
  , Tag (..)
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
import Data.Int
import Data.RRBVector qualified as R
import Data.RRBVector (Vector, (<|), (!))
import Data.Serialize
import Data.Text (Text)
import Data.Text.Encoding

-- | An NBT tag type, in order so that 'toEnum' and 'fromEnum' are useful for serialization and
-- deserialization.
data Type
  = EndType       -- ^ 0x00 NUL
  | ByteType      -- ^ 0x01 SOH
  | ShortType     -- ^ 0x02 STX
  | IntType       -- ^ 0x03 ETX
  | LongType      -- ^ 0x04 EOT
  | FloatType     -- ^ 0x05 ENQ
  | DoubleType    -- ^ 0x06 ACK
  | ByteArrayType -- ^ 0x07 BEL
  | StringType    -- ^ 0x08 BS
  | ListType      -- ^ 0x09 HT
  | CompoundType  -- ^ 0x0a LF
  | IntArrayType  -- ^ 0x0b VT
  | LongArrayType -- ^ 0x0c FF
  deriving (Show, Eq, Ord, Enum)

-- | The main NBT type, with support for serialization and deserialization. It couples a 'Text' label with a 'Tag'.
-- 
-- When serialized: 1-byte unsigned 'Type' + 2-byte unsigned integer length + N bytes utf-8 text + 'Tag'
data Nbt = Nbt
  { label :: Text
  , tag :: Tag
  }
  deriving (Show, Eq, Ord)

-- | An NBT tag, responsible for storing the actual data.
data Tag
  = Byte      Int8           -- ^ 1-byte signed integer
  | Short     Int16          -- ^ 2-byte signed integer
  | Int       Int32          -- ^ 4-byte signed integer
  | Long      Int64          -- ^ 8-byte signed integer
  | Float     Float          -- ^ 4-byte float
  | Double    Double         -- ^ 8-byte double
  | ByteArray (Vector Int8)  -- ^ 4-byte signed integer length + N 1-byte signed integers
  | String    Text           -- ^ 2-byte unsigned integer length + N bytes utf-8 text
  | List      (Vector Tag)   -- ^ 1-byte unsigned 'Type' + 4-byte signed integer length + N homogenous tags
  | Compound  (Vector Nbt)   -- ^ N heterogenous NBT elements punctuated by an 'EndType'
  | IntArray  (Vector Int32) -- ^ 4-byte signed integer length + N 4-byte signed integers
  | LongArray (Vector Int64) -- ^ 4-byte signed integer length + N 8-byte signed integers
  deriving (Show, Eq, Ord)

instance Serialize Type where
  get = toEnum . fromIntegral <$> getWord8
  put = putWord8 . fromIntegral . fromEnum

instance Serialize Nbt where
  get = get >>= getNbtByType
  put (Nbt n d) = put (typeOf d) >> putString n >> putTag d

putTag :: Tag -> Put
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
        let xt = typeOf (ts ! 0)
        let xs = R.drop 1 ts
        if all (\e -> typeOf e == xt) xs then
          pure xt
        else
          error "attempted to write heterogenous list"
    put ty
    putArray putTag ts
  Compound ts -> traverse_ put ts >> put EndType
  IntArray is -> putArray put is
  LongArray is -> putArray put is
  where
    putArray :: (e -> Put) -> Vector e -> Put
    putArray putter a = do
      put (fromIntegral (length a) :: Int32)
      traverse_ putter a

-- | Get the type of an NBT tag.
typeOf :: Tag -> Type
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

getByType :: Type -> Get Tag
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
  CompoundType -> Compound <$> getCompound
  IntArrayType -> IntArray <$> getArray get
  LongArrayType -> LongArray <$> getArray get
  where
    getCompound :: Get (Vector Nbt)
    getCompound = get >>= \case
      EndType -> pure $ R.empty
      ty -> (<|) <$> getNbtByType ty <*> getCompound

    getArray :: Get e -> Get (Vector e)
    getArray getter = do
      len <- get :: Get Int32
      elts <- replicateM (fromIntegral len) getter
      pure . R.fromList $ elts

getNbtByType :: Type -> Get Nbt
getNbtByType ty = Nbt <$> getString <*> getByType ty

getString :: Get Text
getString = do
  len <- get :: Get Int16
  decodeUtf8 <$> getByteString (fromIntegral len)

putString :: Text -> Put
putString t = do
  let b = encodeUtf8 t
  put (fromIntegral (B.length b) :: Int16)
  putByteString b

-- | Read an uncompressed NBT file. Exceptions are thrown as usual if the internal call to 'Data.ByteString.readFile' fails,
-- and the 'Either' message is that returned by 'Data.Serialize.decode'.
readUncompressed :: FilePath -> IO (Either String Nbt)
readUncompressed fp = decode <$> B.readFile fp

-- | Write an uncompressed NBT file.
writeUncompressed :: FilePath -> Nbt -> IO ()
writeUncompressed fp nbt = B.writeFile fp $ runPut (put nbt)

-- | Read a GZip-compressed NBT file. Exceptions are thrown as usual if the internal call to 'Data.ByteString.Lazy.readFile' fails,
-- and the 'Either' message is that returned by 'Data.Serialize.decode'.
readCompressed :: FilePath -> IO (Either String Nbt)
readCompressed fp = do
  bs <- GZ.decompress <$> BL.readFile fp
  pure . decode $ B.toStrict bs

-- | Write a GZip-compressed NBT file.
writeCompressed :: FilePath -> Nbt -> IO ()
writeCompressed fp nbt = do
  let bs = GZ.compress . BL.fromStrict $ runPut (put nbt)
  B.writeFile fp (B.toStrict bs)

