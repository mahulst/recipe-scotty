{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Helpers where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, ReaderT, asks)
import           Control.Monad.Trans.Class   (MonadTrans, lift)
import           Data.Text.Lazy              (Text)
import qualified Database.Persist            as DB
import qualified Database.Persist.Postgresql as DB
import           Network.HTTP.Types.Status            (notFound404)


import           Data.Aeson                  (Value (Null))
import           Web.Scotty.Trans            (ActionT, json, status)

data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)

type Action = TypedAction ()

type TypedAction a = ActionT Error ConfigM a

data Config = Config
  { environment :: Environment
  , pool        :: DB.ConnectionPool
  }

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type Error = Text

runDB :: (MonadTrans t, MonadIO (t ConfigM)) => DB.SqlPersistT IO a -> t ConfigM a
runDB q = do
  p <- lift (asks pool)
  liftIO (DB.runSqlPool q p)

notFoundA :: Action
notFoundA = do
  status notFound404
  json Null


toKey :: DB.ToBackendKey DB.SqlBackend a =>
  Integer -> DB.Key a
toKey i = DB.toSqlKey (fromIntegral (i :: Integer))