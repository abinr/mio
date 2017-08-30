{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Text
import Data.Scientific
import qualified Data.ByteString.Char8 as B

data Event = Credit | Debit deriving Show

data Entry = Entry
  { event :: Event
  , price :: Scientific
  } deriving Show

data SavedEntry = SavedEntry
  { id :: Int
  , savedEvent :: Event
  , savedPrice :: Scientific
  , time :: LocalTime
  } deriving Show

{-- when could a postgres enum be null? --}
instance FromField Event where
  fromField f Nothing = returnError UnexpectedNull f "empty event field"
  fromField f (Just x) =
    case B.unpack x of
      "credit" -> return Credit
      "debit" -> return Debit

instance ToField Event where
  toField x =
    case x of
      Credit -> Escape "credit"
      Debit -> Escape "debit"

instance FromRow SavedEntry where
  fromRow = SavedEntry
    <$> field
    <*> field
    <*> field
    <*> field

instance ToRow Entry where
  toRow d =
    [ toField (event d)
    , toField (price d)
    ]

main :: IO ()
main = do
  insertEvent $ Entry Credit 123.34
  mapM_ print =<< queryEvents


insertEvent e = do
  conn <- makeConn
  execute conn "insert into entries (event, price) values (?,?)" e

queryEvents :: IO [SavedEntry]
queryEvents = do
  conn <- makeConn
  query_ conn "select id, event, price, time from entries" :: IO [SavedEntry]


makeConn :: IO Connection
makeConn =
  connect defaultConnectInfo { connectDatabase = "zstore" }
