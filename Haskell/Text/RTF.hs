{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Text.RTF
  (readFile,
   readByteString)
  where

import qualified BinaryFiles as BF
import qualified Control.Monad.Identity as MTL
import qualified Data.ByteString as BS
import qualified Data.Char as Ch
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Text as T
import qualified Data.Typeable as Ty
import qualified Numeric as Nu

import Prelude
  (Bool(..),
   not,
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


data Action
  = ControlWordAction T.Text (Maybe Int)
  | ControlSymbolAction Char
  | GroupStartAction
  | GroupEndAction
  | TextAction T.Text
  | DataAction BS.ByteString
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
    Group body ->
      let paragraphs = parseBody body
      in Just $ RTF {
                    rtfParagraphs = paragraphs
                  }
    _ -> Nothing


parseBody :: [LowRTF] -> [Paragraph]
parseBody items = MTL.runIdentity $ do
  yieldActions items
  C.=$= skipHead
  C.=$= processEscapes
  C.=$= separateParagraphs
  C.=$= C.concatMapM (\actions ->
                  C.sourceList actions
                  C.=$= processParagraph
                  C.$$ C.consume)
  C.$$ C.consume


yieldActions :: [LowRTF] -> C.Source MTL.Identity Action
yieldActions [] = return ()
yieldActions (low:rest) = do
  case low of
    ControlWord text maybeInt -> do
      C.yield $ ControlWordAction text maybeInt
    ControlSymbol c -> do
      C.yield $ ControlSymbolAction c
    Group items -> do
      C.yield $ GroupStartAction
      yieldActions items
      C.yield $ GroupEndAction
    Text text -> do
      C.yield $ TextAction text
    Data data' -> do
      C.yield $ DataAction data'
  yieldActions rest


skipHead :: C.Conduit Action MTL.Identity Action
skipHead = do
  let loopInHead = do
        maybeAction <- C.await
        case maybeAction of
          Just (TextAction text) -> do
            let (_, point) = T.breakOn "\n\n" text
            if T.null point
              then loopInHead
              else do
                let after = T.drop 2 point
                if T.null after
                  then return ()
                  else C.yield $ TextAction after
                loopInBody
          Just _ -> loopInHead
          Nothing -> return ()
      loopInBody = do
        maybeAction <- C.await
        case maybeAction of
          Just action -> do
            C.yield action
            loopInBody
          Nothing -> return ()
  loopInHead


processEscapes :: C.Conduit Action MTL.Identity Action
processEscapes = do
  let loop textSoFar = do
        maybeAction <- C.await
        case maybeAction of
          Just (ControlSymbolAction '\'') -> do
            maybeNextAction <- C.await
            case maybeNextAction of
              Just action -> do
                gotActionAfterEscape textSoFar action
              Nothing -> do
                yieldIfApplicable textSoFar
          Just action -> do
            gotAction textSoFar action
          Nothing -> do
            yieldIfApplicable textSoFar
      gotAction textSoFar action = do
        case action of
          TextAction textHere -> do
            loop $ T.concat [textSoFar, textHere]
          _ -> do
            yieldIfApplicable textSoFar
            C.yield action
            loop ""
      gotActionAfterEscape textSoFar action = do
        case action of
          TextAction textHere -> do
            let escapeSequence = T.take 2 textHere
                restText = T.drop 2 textHere
                maybeEscapedText = decodeEscape escapeSequence
                outputText =
                  case maybeEscapedText of
                    Nothing ->
                      T.concat [textSoFar, textHere]
                    Just escapedText ->
                      T.concat [textSoFar, escapedText, restText]
            loop outputText
          _ -> do
            yieldIfApplicable textSoFar
            gotAction "" action
      yieldIfApplicable textSoFar = do
        if not $ T.null textSoFar
          then C.yield $ TextAction textSoFar
          else return ()
  loop ""


decodeEscape :: T.Text -> Maybe T.Text
decodeEscape text
  | (T.length text == 2) =
    case Nu.readHex $ T.unpack text of
      [(codepoint, "")] -> Just $ T.pack [Ch.chr codepoint]
      _ -> Nothing
  | True = Nothing


separateParagraphs :: C.Conduit Action MTL.Identity [Action]
separateParagraphs = do
  let loop soFar = do
        maybeAction <- C.await
        case maybeAction of
          Just action@(ControlSymbolAction '\n') -> do
            maybeNextAction <- C.await
            case maybeNextAction of
              Just (ControlSymbolAction '\n') -> do
                C.yield soFar
                loop []
              Just nextAction -> do
                loop (soFar ++ [action, nextAction])
              Nothing -> do
                C.yield (soFar ++ [action])
          Just action -> do
            loop (soFar ++ [action])
          Nothing -> do
            case soFar of
             [] -> return ()
             _ -> C.yield soFar
  loop []


processParagraph :: C.Conduit Action MTL.Identity Paragraph
processParagraph = do
  let loop textSoFar = do
        maybeAction <- C.await
        case maybeAction of
          Just (TextAction text) -> do
            loop (T.concat [textSoFar, text])
          Nothing -> do
            C.yield $ Paragraph {
                          paragraphText = textSoFar
                        }
          _ -> do
            loop textSoFar
  loop ""

