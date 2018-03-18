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
import           Data.Aeson                  (FromJSON, ToJSON, defaultOptions,
                                              fieldLabelModifier, genericToJSON,
                                              toJSON)
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

import qualified Recipe

import           Data.Char                   (toLower)
import           Network.HTTP.Types.Status   (created201)
import           Web.Scotty.Trans            (json, jsonData, param, status)

share
  [mkMigrate "migrateList", mkPersist sqlSettings]
  [persistLowerCase|
DinnerListRow json
  day Int
  recipe Recipe.RecipeId
  created UTCTime Maybe default=now()
  deriving Show

DinnerList json
  name Text Maybe
  weekNumber Int
  recipes [DinnerListRowId]
  created UTCTime Maybe default=now()
  deriving Show Generic
|]

removePrefix :: Int -> String -> String
removePrefix prefixLength s =
  let capitalized :: String -> String
      capitalized (head:tail) = toLower head : tail
      capitalized []          = []
  in capitalized $ drop prefixLength s

data DinnerListWithRecipes = DinnerListWithRecipes
  { dLWRows    :: [DB.Entity DinnerListRow]
  , dLWRecipes :: [DB.Entity Recipe.Recipe]
  } deriving (Generic, Show)

instance ToJSON DinnerListWithRecipes where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = (removePrefix 3)}

instance FromJSON DinnerListWithRecipes

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
  maybeRecipe :: Maybe Recipe.Recipe <- runDB (DB.get (toKey recipeId))
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

getListA :: Action
getListA = do
  listId <- param "id"
  maybeList :: Maybe DinnerList <- runDB $ DB.get (toKey listId)
  case maybeList of
    Nothing -> notFoundA
    Just l -> do
      let rowIds = dinnerListRecipes l
      rows <- runDB (DB.selectList [DinnerListRowId DB.<-. rowIds] [])
      let recipeIds = fmap (\row -> dinnerListRowRecipe (DB.entityVal row)) rows
      recipes <- runDB (DB.selectList [Recipe.RecipeId DB.<-. recipeIds] [])
      let a = DinnerListWithRecipes rows recipes
      json a

--
showAllListsA :: Action
showAllListsA = do
  lists :: [DB.Entity DinnerList] <- runDB (DB.selectList [] [DB.Asc DinnerListCreated, DB.LimitTo 10])
  json lists
