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
  (Bool(..),
   Maybe(..),
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
   map,
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
      rtfHeader :: Header,
      rtfParagraphs :: [Paragraph]
    }
  deriving (Show)


data Header =
  Header {
      headerVersion :: Int,
      headerFbidis :: Bool,
      headerCharacterSet :: CharacterSet,
      headerCodepage :: Maybe Int,
      headerFrom :: Maybe From,
      headerDefaultPlainFont :: Maybe Int,
      headerDefaultBidirectionalFont :: Maybe Int,
      headerDefaultStylesheetFonts :: Maybe StylesheetFonts,
      headerDefaultPlainLanguage :: Maybe Int,
      headerDefaultEastAsianLanguage :: Maybe Int,
      headerDefaultMiddleEasternLanguage :: Maybe Int,
      headerFontTable :: Maybe FontTable
    }
  deriving (Show)


data CharacterSet
  = ASCIICharacterSet
  | MacRomanCharacterSet
  | MicrosoftDOSCharacterSet
  | OS2CharacterSet
  deriving (Show)


data From
  = FromText
  | FromHTML Int
  | FromCocoa Int
  deriving (Show)


data StylesheetFonts =
  StylesheetFonts {
      stylesheetFontsEastAsian :: Int,
      stylesheetFontsPlain :: Int,
      stylesheetFontsHigh :: Int,
      stylesheetFontsBidirectional :: Int
    }
  deriving (Show)


data FontTable =
  FontTable {
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
              [(number, "")] -> return $ ControlWord alpha $ Just number
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
    Group content -> do
      (header, body) <- parseHeader content
      paragraphs <- parseBody content
      return $ RTF {
                   rtfHeader = header,
                   rtfParagraphs = paragraphs
                 }
    _ -> Nothing


parseHeader :: [LowRTF] -> Maybe (Header, [LowRTF])
parseHeader low = do
  (version, low) <-
    case low of
      (ControlWord "rtf" (Just 1) : rest) -> return (1, rest)
      _ -> Nothing
  (fbidis, low) <-
    case low of
      (ControlWord "fbidis" Nothing : rest) -> return (True, rest)
      _ -> return (False, low)
  (characterSet, low) <-
    case low of
      (ControlWord "ansi" Nothing : rest) ->
        return (ASCIICharacterSet, rest)
      (ControlWord "mac" Nothing : rest) ->
        return (MacRomanCharacterSet, rest)
      (ControlWord "pc" Nothing : rest) ->
        return (MicrosoftDOSCharacterSet, rest)
      (ControlWord "pca" Nothing : rest) ->
        return (OS2CharacterSet, rest)
      _ -> return (ASCIICharacterSet, low)
  (codepage, low) <-
    case low of
      (ControlWord "ansicpg" (Just codepage) : rest) ->
        return (Just codepage, rest)
      _ -> return (Nothing, low)
  (from, low) <-
    case low of
      (ControlWord "fromtext" Nothing : rest) ->
        return (Just FromText, rest)
      (ControlWord "fromhtml" (Just version) : rest) ->
        return (Just $ FromHTML version, rest)
      (ControlWord "cocoartf" (Just version) : rest) ->
        return (Just $ FromCocoa version, rest)
      _ -> return (Nothing, low)
  (defaultPlainFont, low) <-
    case low of
      (ControlWord "deff" (Just index) : rest) ->
        return (Just index, rest)
      _ -> return (Nothing, low)
  (defaultBidirectionalFont, low) <-
    case low of
      (ControlWord "adeff" (Just index) : rest) ->
        return (Just index, rest)
      _ -> return (Nothing, low)
  (defaultStylesheetFonts, low) <-
    case low of
      (ControlWord "stshfdbch" (Just eastAsianIndex)
       : ControlWord "stshfloch" (Just plainIndex)
       : ControlWord "stshfhich" (Just highIndex)
       : ControlWord "stshfbi" (Just bidirectionalIndex)
       : rest) ->
        return (Just $ StylesheetFonts {
                           stylesheetFontsEastAsian = eastAsianIndex,
                           stylesheetFontsPlain = plainIndex,
                           stylesheetFontsHigh = highIndex,
                           stylesheetFontsBidirectional = bidirectionalIndex
                         },
                rest)
      _ -> return (Nothing, low)
  (defaultPlainLanguage, low) <-
    case low of
      (ControlWord "deflang" (Just index) : rest) ->
        return (Just index, rest)
      _ -> return (Nothing, low)
  (defaultEastAsianLanguage, low) <-
    case low of
      (ControlWord "deflangfe" (Just index) : rest) ->
        return (Just index, rest)
      _ -> return (Nothing, low)
  (defaultMiddleEasternLanguage, low) <-
    case low of
      (ControlWord "adeflang" (Just index) : rest) ->
        return (Just index, rest)
      _ -> return (Nothing, low)
  (fontTable, low) <-
    case low of
      (Group (ControlWord "fonttbl" Nothing : table) : rest) -> do
        table <- parseFontTable table
        return (Just table, rest)
      _ -> return (Nothing, low)
  return (Header {
              headerVersion = version,
              headerFbidis = fbidis,
              headerCharacterSet = characterSet,
              headerCodepage = codepage,
              headerFrom = from,
              headerDefaultPlainFont = defaultPlainFont,
              headerDefaultBidirectionalFont = defaultBidirectionalFont,
              headerDefaultStylesheetFonts = defaultStylesheetFonts,
              headerDefaultPlainLanguage = defaultPlainLanguage,
              headerDefaultEastAsianLanguage = defaultEastAsianLanguage,
              headerDefaultMiddleEasternLanguage = defaultMiddleEasternLanguage,
              headerFontTable = fontTable
            },
          low)


parseFontTable :: [LowRTF] -> Maybe FontTable
parseFontTable low = do
  case low of
    [] -> return ()
    _ -> Nothing
  return FontTable {
           }


{-
\rtf1 \fbidis? <character set> <from>? <deffont> <deflang> <fonttbl>? <filetbl>? <colortbl>? <stylesheet>? <stylerestrictions>? <listtables>? <revtbl>? <rsidtable>? <mathprops>? <generator>?
-}

parseBody :: [LowRTF] -> Maybe [Paragraph]
parseBody low = do
  flatText <- mapM flatten low >>= return . T.concat
  return $ map (\text ->
                  Paragraph {
                      paragraphText = text
                    })
               (T.splitOn "\n\n" flatText)


flatten :: LowRTF -> Maybe T.Text
flatten (ControlWord _ _) = return T.empty
flatten (ControlSymbol '\n') = return "\n"
flatten (ControlSymbol _) = return T.empty
flatten (Group items) = mapM flatten items >>= return . T.concat
flatten (Text text) = return text
flatten (Data _) = return T.empty 

