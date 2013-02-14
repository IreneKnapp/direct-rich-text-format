{-# LANGUAGE DeriveDataTypeable #-}
module Text.RTF
  (readFile,
   read)
  where

import qualified BinaryFiles as BF
import qualified Data.ByteString as BS
import qualified Data.Char as Ch
import qualified Data.Text as T
import qualified Data.Typeable as Ty

import Prelude
  (Maybe(..),
   Either(..),
   Int,
   IO,
   Char,
   FilePath,
   Show(..),
   Integral(..),
   fromIntegral,
   return,
   ($),
   (.),
   (>>=),
   (>),
   (<),
   (==),
   (>=),
   (<=),
   (/=),
   (&&),
   (||))


data LowRTF
  = ControlWord T.Text (Maybe Int)
  | ControlSymbol Char
  | Group [LowRTF]
  | Text T.Text
  | Data BS.ByteString
  deriving (Show)


data Failure = Failure
  deriving (Show, Ty.Typeable)
instance BF.SerializationFailure Failure where
  toSerializationFailure failure =
    BF.SomeSerializationFailure failure
  fromSerializationFailure (BF.SomeSerializationFailure failure) =
    Ty.cast failure


readFile :: FilePath -> IO (Maybe LowRTF)
readFile filePath = do
  eitherResult <- BF.runDeserializationFromFile deserialize filePath
  case eitherResult of
    Left _ -> return Nothing
    Right result -> return $ Just result


read :: BS.ByteString -> Maybe LowRTF
read data' =
  let eitherResult = BF.runDeserializationFromByteString deserialize data'
  in case eitherResult of
       Left _ -> Nothing
       Right result -> Just result

deserialize :: BF.Deserialization LowRTF
deserialize = do
  c <- deserializeCharacter
  result <- case c of
              '{' -> deserializeGroup
              '\\' -> deserializeEscape
              _ -> BF.throw Failure
  isEOF <- BF.isEOF
  if isEOF
    then return result
    else BF.throw Failure


deserializeCharacter :: BF.Deserialization Char
deserializeCharacter = do
  byte <- BF.read 1 >>= return . BS.head
  if (byte > 0) && (byte < 128)
    then return $ Ch.chr $ fromIntegral byte
    else BF.throw Failure


deserializeGroup :: BF.Deserialization LowRTF
deserializeGroup = do
  let finish itemsSoFar textSoFar =
        if T.null textSoFar
          then itemsSoFar
          else itemsSoFar ++ [Text textSoFar]
      loop itemsSoFar textSoFar = do
        c <- deserializeCharacter
        case c of
          '}' -> return $ finish itemsSoFar textSoFar
          '{' -> do
            subgroup <- deserializeGroup
            loop (finish itemsSoFar textSoFar ++ subgroup) ""
          '\\' -> do
            escape <- deserializeEscape
            loop (finish itemsSoFar textSoFar ++ escape) ""
          _ -> loop itemsSoFar (T.snoc textSoFar c)


deserializeEscape :: BF.Deserialization LowRTF
deserializeEscape = do

