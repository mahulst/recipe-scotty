{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Helpers where

import           Control.Monad.Trans.Class   (MonadTrans, lift)
import           Data.Text.Lazy              (Text)
import qualified Database.Persist            as DB
import qualified Database.Persist.Postgresql as DB
import           Web.Scotty.Trans            (ActionT, delete, get, notFound,
                                              post, put)

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, ReaderT, asks,
                                              runReaderT)

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
