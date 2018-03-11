{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Helpers where

import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ActionT, Options, ScottyT, defaultHandler, delete,
  get, json, jsonData, middleware, notFound, param, post, put, scottyOptsT,
  settings, showError, status, verbose)
import Control.Monad.Trans.Class (MonadTrans, lift)

import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)


data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)


type Action = TypedAction ()

type TypedAction a = ActionT Error ConfigM a


data Config = Config
  { environment :: Environment
  , pool :: DB.ConnectionPool
  }

newtype ConfigM a = ConfigM
 { runConfigM :: ReaderT Config IO a
 } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type Error = Text

runDB :: (MonadTrans t, MonadIO (t ConfigM)) =>
  DB.SqlPersistT IO a -> t ConfigM a
runDB q = do
  p <- lift (asks pool)
  liftIO (DB.runSqlPool q p)

