{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module List where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.List                   (concatMap)
import           Data.Text                   (Text)
import           Data.Time.Clock             (UTCTime)
import           Data.Time.Clock.POSIX       (getCurrentTime)
import qualified Database.Persist            as DB
import qualified Database.Persist.Postgresql as DB
import           Database.Persist.TH         (mkMigrate, mkPersist,
                                              persistLowerCase, share,
                                              sqlSettings)
import           GHC.Generics
import           Helpers                     (Action, TypedAction, notFoundA,
                                              runDB, toKey)

import           Recipe                      (Recipe, RecipeId)

import           Network.HTTP.Types.Status   (created201)
import           Web.Scotty.Trans            (json, jsonData, param, status)

share
  [mkMigrate "migrateList", mkPersist sqlSettings]
  [persistLowerCase|
DinnerListRow json
  day Int
  recipe RecipeId
  created UTCTime Maybe default=now()
  deriving Show

DinnerList json
  name Text Maybe
  weekNumber Int
  recipes [DinnerListRowId]
  created UTCTime Maybe default=now()
  deriving Show Generic
|]

insertList :: DinnerList -> TypedAction (DB.Key DinnerList)
insertList list = runDB (DB.insert list)

updateList :: DinnerList -> Integer -> Action
updateList list id = runDB (DB.replace (toKey id) list)

insertListRow :: DinnerListRow -> TypedAction (DB.Key DinnerListRow)
insertListRow listRow = runDB (DB.insert listRow)

postNewListA :: Action
postNewListA = do
  time <- liftIO getCurrentTime
  let createdList = DinnerList Nothing 1 [] (Just time)
  _ <- insertList createdList
  status created201
  json (createdList :: DinnerList)

addRecipeToListA :: Action
addRecipeToListA = do
  recipeId <- param "recipe"
  listId <- param "list"
  time <- liftIO getCurrentTime
  maybeList :: Maybe DinnerList <- runDB (DB.get (toKey listId))
  maybeRecipe :: Maybe Recipe <- runDB (DB.get (toKey recipeId))
  let newDinnerListRow = fmap (\_ -> DinnerListRow 1 (toKey recipeId) (Just time)) maybeRecipe
  case maybeList of
    Nothing -> notFoundA
    Just l ->
      case newDinnerListRow of
        Nothing -> notFoundA
        Just r -> do
          rowId <- insertListRow r
          let allRecipes = dinnerListRecipes l ++ [rowId]
          let l' = l {dinnerListRecipes = allRecipes}
          _ <- updateList l' listId
          status created201
          json l'

--
showAllListsA :: Action
showAllListsA = do
  lists :: [DB.Entity DinnerList] <- runDB (DB.selectList [] [DB.Asc DinnerListCreated, DB.LimitTo 10])
  json lists
