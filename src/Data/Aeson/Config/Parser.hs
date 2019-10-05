{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Aeson.Config.Parser (
  Parser
, runParser

, typeMismatch
, withObject
, withText
, withString
, withArray
, withNumber
, withBool

, explicitParseField
, explicitParseFieldMaybe

, Aeson.JSONPathElement(..)
, (<?>)

, Value(..)
, Object
, Array

, liftParser

, fromAesonPath
, formatPath
) where

import           Control.Monad
import           Control.Applicative
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
import           Data.Monoid ((<>))
import           Data.Scientific
import           Data.Set (Set, notMember)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HashMap
import           Data.Aeson.Types (Value(..), Object, Array)
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson.Internal (IResult(..), iparse)
import qualified Data.Aeson.Internal as Aeson

-- This is needed so that we have an Ord instance for aeson < 1.2.4.
data JSONPathElement = Key Text | Index Int
  deriving (Eq, Show, Ord)

type JSONPath = [JSONPathElement]

fromAesonPath :: Aeson.JSONPath -> JSONPath
fromAesonPath = reverse . map fromAesonPathElement

fromAesonPathElement :: Aeson.JSONPathElement -> JSONPathElement
fromAesonPathElement e = case e of
  Aeson.Key k -> Key k
  Aeson.Index n -> Index n

newtype Parser a = Parser {unParser :: WriterT (Set JSONPath) Aeson.Parser a}
  deriving (Functor, Applicative, Alternative, Monad, Fail.MonadFail)

liftParser :: Aeson.Parser a -> Parser a
liftParser = Parser . lift

runParser :: (Value -> Parser a) -> Value -> Either String (a, [String])
runParser p v = case iparse (runWriterT . unParser <$> p) v of
  IError path err -> Left ("Error while parsing " ++ formatPath (fromAesonPath path) ++ " - " ++ err)
  ISuccess (a, consumed) -> Right (a, map formatPath (determineUnconsumed consumed v))

formatPath :: JSONPath -> String
formatPath = go "$" . reverse
  where
    go :: String -> JSONPath -> String
    go acc path = case path of
      [] -> acc
      Index n : xs -> go (acc ++ "[" ++ show n ++ "]") xs
      Key key : xs -> go (acc ++ "." ++ T.unpack key) xs

determineUnconsumed :: Set JSONPath -> Value -> [JSONPath]
determineUnconsumed ((<> Set.singleton []) -> consumed) = Set.toList . execWriter . go []
  where
    go :: JSONPath -> Value -> Writer (Set JSONPath) ()
    go path value
      | path `notMember` consumed = tell (Set.singleton path)
      | otherwise = case value of
          Number _ -> return ()
          String _ -> return ()
          Bool _ -> return ()
          Null -> return ()
          Object o -> do
            forM_ (HashMap.toList o) $ \ (k, v) -> do
              unless ("_" `T.isPrefixOf` k) $ do
                go (Key k : path) v
          Array xs -> do
            forM_ (zip [0..] $ V.toList xs) $ \ (n, v) -> do
              go (Index n : path) v

(<?>) :: Parser a -> Aeson.JSONPathElement -> Parser a
(<?>) (Parser (WriterT p)) e = do
  Parser (WriterT (p Aeson.<?> e)) <* markConsumed (fromAesonPathElement e)

markConsumed :: JSONPathElement -> Parser ()
markConsumed e = do
  path <- getPath
  Parser $ tell (Set.singleton $ e : path)

getPath :: Parser JSONPath
getPath = liftParser $ Aeson.parserCatchError empty $ \ path _ -> return (fromAesonPath path)

explicitParseField :: (Value -> Parser a) -> Object -> Text -> Parser a
explicitParseField p o key = case HashMap.lookup key o of
  Nothing -> fail $ "key " ++ show key ++ " not present"
  Just v  -> p v <?> Aeson.Key key

explicitParseFieldMaybe :: (Value -> Parser a) -> Object -> Text -> Parser (Maybe a)
explicitParseFieldMaybe p o key = case HashMap.lookup key o of
  Nothing -> pure Nothing
  Just v  -> Just <$> p v <?> Aeson.Key key

typeMismatch :: String -> Value -> Parser a
typeMismatch expected = liftParser . Aeson.typeMismatch expected

withObject :: (Object -> Parser a) -> Value -> Parser a
withObject p (Object o) = p o
withObject _ v = typeMismatch "Object" v

withText :: (Text -> Parser a) -> Value -> Parser a
withText p (String s) = p s
withText _ v = typeMismatch "String" v

withString :: (String -> Parser a) -> Value -> Parser a
withString p = withText (p . T.unpack)

withArray :: (Array -> Parser a) -> Value -> Parser a
withArray p (Array xs) = p xs
withArray _ v = typeMismatch "Array" v

withNumber :: (Scientific -> Parser a) -> Value -> Parser a
withNumber p (Number n) = p n
withNumber _ v = typeMismatch "Number" v

withBool :: (Bool -> Parser a) -> Value -> Parser a
withBool p (Bool b) = p b
withBool _ v = typeMismatch "Boolean" v
