{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Text.RTF
  (readFile,
   readByteString)
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
   Read(..),
   reads,
   read,
   Show(..),
   Integral(..),
   fromIntegral,
   return,
   mapM,
   mapM_,
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
   (||),
   (++))


data LowRTF
  = ControlWord T.Text (Maybe Int)
  | ControlSymbol Char
  | Group [LowRTF]
  | Text T.Text
  | Data BS.ByteString
  deriving (Show)


data RTF =
  RTF {
      rtfParagraphs :: [Paragraph]
    }
  deriving (Show)


data Paragraph =
  Paragraph {
      paragraphText :: T.Text
    }
  deriving (Show)


data Failure = Failure
  deriving (Show, Ty.Typeable)
instance BF.SerializationFailure Failure where
  toSerializationFailure failure =
    BF.SomeSerializationFailure failure
  fromSerializationFailure (BF.SomeSerializationFailure failure) =
    Ty.cast failure


readFile :: FilePath -> IO (Maybe RTF)
readFile filePath = do
  eitherResult <- BF.runDeserializationFromFile deserialize filePath
  case eitherResult of
    Left _ -> return Nothing
    Right result -> return $ Just result


readByteString :: BS.ByteString -> Maybe RTF
readByteString data' =
  let eitherResult = BF.runDeserializationFromByteString deserialize data'
  in case eitherResult of
       Left _ -> Nothing
       Right result -> Just result

deserialize :: BF.Deserialization RTF
deserialize = do
  c <- deserializeCharacter
  result <- case c of
              '{' -> deserializeGroup
              _ -> BF.throw Failure
  isEOF <- BF.isEOF
  if isEOF
    then case parse result of
           Nothing -> BF.throw Failure
           Just result' -> return result'
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
          '}' -> return $ Group $ finish itemsSoFar textSoFar
          '{' -> do
            subgroup <- deserializeGroup
            loop (finish itemsSoFar textSoFar ++ [subgroup]) ""
          '\\' -> do
            escape <- deserializeEscape
            loop (finish itemsSoFar textSoFar ++ [escape]) ""
          _ -> loop itemsSoFar (T.snoc textSoFar c)
  loop [] T.empty


deserializeEscape :: BF.Deserialization LowRTF
deserializeEscape = do
  let loopAlpha soFar = do
        position <- BF.tell
        c <- deserializeCharacter
        if Ch.isAlpha c
          then loopAlpha $ T.concat [soFar, T.singleton c]
          else if (Ch.isDigit c) || (c == '-')
                 then loopDigit soFar $ T.singleton c
                 else do
                   if c == ' '
                     then return ()
                     else BF.seek BF.OffsetFromStart position
                   return $ ControlWord soFar Nothing
      loopDigit alpha soFar = do
        position <- BF.tell
        c <- deserializeCharacter
        if Ch.isDigit c
          then loopDigit alpha $ T.concat [soFar, T.singleton c]
          else do
            if c == ' '
              then return ()
              else BF.seek BF.OffsetFromStart position
            case reads $ T.unpack soFar of
              [(number, "")] -> return $ ControlWord soFar $ Just number
              _ -> BF.throw Failure
  c <- deserializeCharacter
  if Ch.isAlpha c
    then do
      result <- loopAlpha $ T.singleton c
      case result of
        ControlWord "bin" (Just count) -> do
          data' <- BF.read count
          return $ Data data'
        _ -> return result
    else return $ ControlSymbol c


parse :: LowRTF -> Maybe RTF
parse low = do
  case low of
    Group (_ : body) -> do
      paragraphs <- parseBody (Group body)
      return $ RTF {
                   rtfParagraphs = paragraphs
                 }
    _ -> Nothing


parseBody :: LowRTF -> Maybe [Paragraph]
parseBody low = do
  flatText <- flatten low
  return [Paragraph {
              paragraphText = flatText
            }]


flatten :: LowRTF -> Maybe T.Text
flatten (ControlWord _ _) = return T.empty
flatten (ControlSymbol _) = return T.empty
flatten (Group items) = mapM flatten items >>= return . T.concat
flatten (Text text) = return text
flatten (Data _) = return T.empty 

