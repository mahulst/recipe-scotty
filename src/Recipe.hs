{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Recipe where

import           Data.Aeson                  (FromJSON, ToJSON)
import           GHC.Generics

import           Control.Monad.IO.Class      (liftIO)
import qualified Database.Persist            as DB
import qualified Database.Persist.Postgresql as DB

import           Data.Text                   (Text)
import           Data.Time.Clock             (UTCTime)
import           Data.Time.Clock.POSIX       (getCurrentTime)
import           Database.Persist.TH         (mkMigrate, mkPersist,
                                              persistLowerCase, share,
                                              sqlSettings)
import           Helpers                     (Action, TypedAction, runDB)

import           Network.HTTP.Types.Status   (created201)
import           Web.Scotty.Trans            (delete, get, json, jsonData,
                                              notFound, post, put, status)

share
  [mkMigrate "migrateRecipe", mkPersist sqlSettings]
  [persistLowerCase|
Ingredient json
  name Text
  created UTCTime Maybe default=now()
  deriving Show Generic

IngredientRow json
  amount Int
  unit Text
  ingredient IngredientId
  created UTCTime Maybe default=now()

Recipe json
  name Text
  description Text
  ingredients [IngredientRowId]
  created UTCTime Maybe default=now()
|]

-- Models for API response of a new recipe
data NewIngredientRow = NewIngredientRow
  { newIngredientRowAmount     :: Int
  , newIngredientRowUnit       :: Text
  , newIngredientRowIngredient :: IngredientId
  } deriving (Generic, Show)

instance ToJSON NewIngredientRow

instance FromJSON NewIngredientRow

data NewRecipe = NewRecipe
  { newRecipeName           :: Text
  , newRecipeDescription    :: Text
  , newRecipeIngredientsRow :: [NewIngredientRow]
  } deriving (Generic, Show)

instance ToJSON NewRecipe

instance FromJSON NewRecipe

-- DB Tasks
insertIngredientRow :: IngredientRow -> TypedAction (DB.Key IngredientRow)
insertIngredientRow row = runDB (DB.insert row)

insertRecipe :: Recipe -> TypedAction (DB.Key Recipe)
insertRecipe recipe = runDB (DB.insert recipe)

-- API Handlers
postRecipesA :: Action
postRecipesA = do
  newRecipe <- jsonData
  time <- liftIO getCurrentTime
  let ingredientRows = ingredientRowsFromNew newRecipe time
  ingredientRowIds <- mapM insertIngredientRow ingredientRows
  let recipe = recipeFromNew newRecipe ingredientRowIds time
  insertRecipe recipe
  status created201
  json (newRecipe :: NewRecipe)


-- Convert Api Models to DB Models
recipeFromNew :: NewRecipe -> [(DB.Key IngredientRow)] -> UTCTime -> Recipe
recipeFromNew newRecipe rows time =
  let name = (newRecipeName newRecipe)
      description = (newRecipeName newRecipe)
  in Recipe name description rows (Just time)

ingredientRowsFromNew :: NewRecipe -> UTCTime -> [IngredientRow]
ingredientRowsFromNew newRecipe time =
  let newIngredientRows = (newRecipeIngredientsRow newRecipe)
      ingredientRows =
        fmap
          (\i ->
             let rowAmount = (newIngredientRowAmount i)
                 rowUnit = (newIngredientRowUnit i)
                 rowIngredient = (newIngredientRowIngredient i)
             in IngredientRow rowAmount rowUnit rowIngredient (Just time))
          newIngredientRows
  in ingredientRows
