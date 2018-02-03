{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses        #-}
{-# LANGUAGE OverloadedStrings, Rank2Types, RankNTypes, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Data.Plist.Parser (parsePList) where
import Data.Plist.Types

import           Attoparsec.Time.ByteString        (utcTimeInISO8601)
import           Control.Arrow                     (first)
import           Control.Lens                      (makeLenses, makePrisms)
import           Control.Lens                      (view, (%%=), (%=), (?=))
import           Control.Monad.Except              (MonadError, runExcept)
import           Control.Monad.Except              (throwError)
import           Control.Monad.State.Strict        (MonadState, execStateT)
import           Data.Attoparsec.ByteString.Base64 (base64)
import           Data.Attoparsec.ByteString.Char8  (Parser, Result, decimal)
import           Data.Attoparsec.ByteString.Char8  (double, eitherResult)
import           Data.Attoparsec.ByteString.Char8  (endOfInput, feed, many1)
import           Data.Attoparsec.ByteString.Char8  (parse, sepBy, skipSpace)
import           Data.Attoparsec.ByteString.Char8  (space, takeByteString)
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Base64            as B64
import qualified Data.ByteString.Char8             as BS8
import           Data.DList                        (DList)
import qualified Data.DList                        as DL
import           Data.Foldable                     (toList)
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as HM
import           Data.List                         (splitAt, uncons)
import           Data.Monoid                       (Monoid (..), (<>))
import           Data.Text                         (Text)
import qualified Data.Text.Encoding                as T
import           Xeno.SAX                          (process)

type Dict = HashMap Text PList

data ParseMode = ParsePList
               | ParsingArray    (DList PList)
               | ParsingDict     Dict
               | ParsingKey      ByteString
               | ParsingValueFor Text
               | ParsingAtom     (Result PList)
makePrisms ''ParseMode

dropDOCTYPE :: ByteString -> ByteString
dropDOCTYPE src =
  let lns = BS8.lines src
      (pfx, sf0) = splitAt 1 lns
      (_, suf)   = splitAt 1 sf0
  in BS8.unlines $ pfx ++ suf

instance Show ParseMode where
  showsPrec _ ParsePList = showString "ParsePList"
  showsPrec d (ParsingArray dl) =
    showParen (d > 10) $
      showString "ParsingArray " . showsPrec 10 (toList dl)
  showsPrec d (ParsingDict dic) =
    showParen (d > 10) $
      showString "ParsingDict " . showsPrec 10 dic
  showsPrec d (ParsingKey k) =
    showParen (d > 10) $ showString "ParsingKey " . showsPrec 10 k
  showsPrec d (ParsingValueFor k) =
    showParen (d > 10) $ showString "ParsingValueFor " . showsPrec 10 k
  showsPrec d (ParsingAtom _) =
    showParen (d > 10) $ showString "ParsingAtom _"

data PState = PState { _parseModes  :: [ParseMode]
                     , _parseResult :: Maybe PList
                     }
            deriving (Show)
makeLenses ''PState

integerParser, realParser, stringParser, dateParser, dataParser :: Parser PList
integerParser = Integer               <$> decimal
realParser    = Real                  <$> double
stringParser  = String . T.decodeUtf8 <$> takeByteString
dateParser    = Date                  <$> utcTimeInISO8601
dataParser    = Data . mconcat        <$> bytesParser
  where
    bytesParser =
       skipSpace *> (B64.decodeLenient <$> base64) `sepBy` many1 space
                 <* skipSpace


push :: MonadState PState m => ParseMode -> m ()
push act = parseModes %= (act :)

parsePList :: ByteString -> Either String PList
parsePList =
    either Left (maybe (Left "No result generated!") Right)
  . fmap (view parseResult)
  . runExcept
  . flip execStateT (PState [] Nothing)
  . process
    (const $ return ())
    (const $ const $ return ())
    onTagOpen
    onText
    onTagClose
    (const $ return ())
  . dropDOCTYPE

expectingValue :: (MonadError String m, MonadState PState m) => Bool -> m ()
expectingValue isRoot = pop >>= \case
  Just i@(ParsingValueFor _) -> push i
  Just i@(ParsingArray _) -> push i
  Just i@ParsePList | isRoot -> push i
  Just i -> throwError $ concat ["Current state ", show i, " doesnt' expect value."]
  Nothing -> throwError $ "Expecting EOF but some value given"


onTagOpen :: (MonadError String m, MonadState PState m) => ByteString -> m ()
onTagOpen "plist"   = push $ ParsePList
onTagOpen "array"   = expectingValue True >> push (ParsingArray mempty)
onTagOpen "dict"    = expectingValue True >> mapM_ push [ParsingDict  mempty]
onTagOpen "key"     = push (ParsingKey "")
onTagOpen "true"    = do expectingValue True; push $ ParsingAtom (parse (Bool True <$ endOfInput)  "")
onTagOpen "false"   = do expectingValue True; push $ ParsingAtom (parse (Bool False <$ endOfInput) "")
onTagOpen "integer" = do expectingValue True; push $ ParsingAtom (parse integerParser              "")
onTagOpen "real"    = do expectingValue True; push $ ParsingAtom (parse realParser                 "")
onTagOpen "string"  = do expectingValue True; push $ ParsingAtom (parse stringParser               "")
onTagOpen "date"    = do expectingValue True; push $ ParsingAtom (parse dateParser                 "")
onTagOpen "data"    = do expectingValue True; push $ ParsingAtom (parse dataParser                 "")
onTagOpen tag       = throwError $ "Unsupported tag: " <> show tag

pop :: MonadState PState m => m (Maybe ParseMode)
pop = parseModes %%= maybe (Nothing, []) (first Just) . uncons

finishWithValue :: (MonadError String m, MonadState PState m) => PList -> m ()
finishWithValue val = pop >>= \case
  Just ParsePList ->
    case val of
      Array {} -> parseResult ?= val
      Dict  {} -> parseResult ?= val
      _        -> throwError $ "PList must be dictionary or array, but got: " <> show val
  Just (ParsingArray dl) -> push $ ParsingArray (dl `DL.snoc` val)
  Just (ParsingValueFor k) -> pop >>= \case
    Just (ParsingDict dic) -> do
      push $ ParsingDict (HM.insert k val dic)
    _ -> throwError "No dictionary parsing in operation!"
  i -> throwError $
       concat [ "Unexpected value: ", show val, "; but current state is: ", show i ]

onText :: (MonadError String m, MonadState PState m) => ByteString -> m ()
onText txt = pop >>= \case
  Just (ParsingKey k) -> push $ ParsingKey $ k <> txt
  Just (ParsingAtom p) -> push $ ParsingAtom (p `feed` txt)
  e -> maybe (return ()) push e

onTagClose :: (MonadError String m, MonadState PState m) => ByteString -> m ()
onTagClose "key"   = pop >>= \case
  Just (ParsingKey bs) -> do
    let k = T.decodeUtf8 bs
    push $ ParsingValueFor k
  e -> throwError ("Premature end of key: current state is " <> show e)
onTagClose "array" = pop >>= \case
  Just (ParsingArray dl) -> finishWithValue $ Array $ toList dl
  e -> throwError ("Premature end of array: current state is " <> show e)
onTagClose "dict" = pop >>= \case
  Just (ParsingDict dic) -> finishWithValue $ Dict dic
  e -> throwError ("Premature end of dict: current state is " <> show e)
onTagClose "plist" = return ()
onTagClose tag
  | tag `elem` ["integer", "real", "true", "false", "string", "data", "date"]
    = pop >>= \case
    Just (ParsingAtom parser) ->
      either throwError finishWithValue $ eitherResult $ parser `feed` ""
    e -> throwError ("Premature end of atom: current state is " <> show e)
  | otherwise = throwError $ "Unsupported tag: " <> show tag

