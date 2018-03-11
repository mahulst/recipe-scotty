{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Api where

import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Reader                 (ReaderT, runReaderT)
import           Data.Aeson                           (Value (Null), object,
                                                       (.=))
import           Data.Default                         (def)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (encodeUtf8)
import qualified Database.Persist                     as DB
import qualified Database.Persist.Postgresql          as DB
import           Helpers                              (Action, Config (..),
                                                       ConfigM (..),
                                                       Environment (..), Error,
                                                       runDB)
import           Models                               (Task, TaskId, migrateAll)
import           Network.HTTP.Types.Status            (created201,
                                                       internalServerError500,
                                                       notFound404)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Settings,
                                                       defaultSettings,
                                                       setFdCacheDuration,
                                                       setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Recipe                               (migrateRecipe,
                                                       postRecipesA)
import           System.Environment                   (lookupEnv)
import           Web.Scotty.Trans                     (Options, ScottyT,
                                                       defaultHandler, delete,
                                                       get, json, jsonData,
                                                       middleware, notFound,
                                                       param, post, put,
                                                       scottyOptsT, settings,
                                                       showError, status,
                                                       verbose)

migrateThing :: DB.Migration -> IO ()
migrateThing thing = do
  c <- getConfig
  liftIO $ flip DB.runSqlPersistMPool (pool c) $ DB.runMigration thing

startServer :: IO ()
startServer = do
  c <- getConfig
  migrateSchema c
  runApplication c

migrateSchema :: Config -> IO ()
migrateSchema c = liftIO $ flip DB.runSqlPersistMPool (pool c) $ DB.runMigration migrateAll

getConfig :: IO Config
getConfig = do
  e <- getEnvironment
  p <- getPool e
  return Config {environment = e, pool = p}

getEnvironment :: IO Environment
getEnvironment = do
  m <- lookupEnv "SCOTTY_ENV"
  let e =
        case m of
          Nothing -> Development
          Just s  -> read s
  return e

getPool :: Environment -> IO DB.ConnectionPool
getPool e = do
  s <- getConnectionString e
  let n = getConnectionSize e
  case e of
    Development -> runStdoutLoggingT (DB.createPostgresqlPool s n)
    Production  -> runStdoutLoggingT (DB.createPostgresqlPool s n)
    Test        -> runNoLoggingT (DB.createPostgresqlPool s n)

getConnectionString :: Environment -> IO DB.ConnectionString
getConnectionString e = do
  m <- lookupEnv "DATABASE_URL"
  let s =
        case m of
          _ -> getDefaultConnectionString e
  return s

getDefaultConnectionString :: Environment -> DB.ConnectionString
getDefaultConnectionString Development = "host=localhost port=5432 user=postgres dbname=recipe_development"
getDefaultConnectionString Production = "host=localhost port=5432 user=postgres dbname=recipe_production"
getDefaultConnectionString Test = "host=localhost port=5432 user=postgres dbname=recipe_test"

createConnectionString :: [(T.Text, T.Text)] -> DB.ConnectionString
createConnectionString l =
  let f (k, v) = T.concat [k, "=", v]
  in encodeUtf8 (T.unwords (map f l))

getConnectionSize :: Environment -> Int
getConnectionSize Development = 1
getConnectionSize Production  = 8
getConnectionSize Test        = 1

runApplication :: Config -> IO ()
runApplication c = do
  o <- getOptions (environment c)
  let r m = runReaderT (runConfigM m) c
      app = application c
  scottyOptsT o r app

getOptions :: Environment -> IO Options
getOptions e = do
  s <- getSettings e
  return
    def
    { settings = s
    , verbose =
        case e of
          Development -> 1
          Production  -> 0
          Test        -> 0
    }

getSettings :: Environment -> IO Settings
getSettings e = do
  let s = defaultSettings
      s' =
        case e of
          Development -> setFdCacheDuration 0 s
          Production  -> s
          Test        -> s
  m <- getPort
  let s'' =
        case m of
          Nothing -> s'
          Just p  -> setPort p s'
  return s''

getPort :: IO (Maybe Int)
getPort = do
  m <- lookupEnv "PORT"
  let p =
        case m of
          Nothing -> Nothing
          Just s  -> Just (read s)
  return p

application :: Config -> ScottyT Error ConfigM ()
application c = do
  let e = environment c
  middleware (loggingM e)
  defaultHandler (defaultH e)
  get "/tasks" getTasksA
  post "/tasks" postTasksA
  post "/recipes" postRecipesA
  get "/tasks/:id" getTaskA
  put "/tasks/:id" putTaskA
  delete "/tasks/:id" deleteTaskA
  notFound notFoundA

loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production  = logStdout
loggingM Test        = id

defaultH :: Environment -> Error -> Action
defaultH e x = do
  status internalServerError500
  let o =
        case e of
          Development -> object ["error" .= showError x]
          Production  -> Null
          Test        -> object ["error" .= showError x]
  json o

getTasksA :: Action
getTasksA = do
  ts <- runDB (DB.selectList [] [])
  json (ts :: [DB.Entity Task])

postTasksA :: Action
postTasksA = do
  t <- jsonData
  runDB (DB.insert_ t)
  status created201
  json (t :: Task)

getTaskA :: Action
getTaskA = do
  i <- param "id"
  m <- runDB (DB.get (toKey i))
  case m of
    Nothing -> notFoundA
    Just t  -> json (t :: Task)

putTaskA :: Action
putTaskA = do
  i <- param "id"
  t <- jsonData
  runDB (DB.repsert (toKey i) t)
  json (t :: Task)

deleteTaskA :: Action
deleteTaskA = do
  i <- param "id"
  runDB (DB.delete (toKey i :: TaskId))
  json Null

toKey :: DB.ToBackendKey DB.SqlBackend a => Integer -> DB.Key a
toKey i = DB.toSqlKey (fromIntegral (i :: Integer))

notFoundA :: Action
notFoundA = do
  status notFound404
  json Null
