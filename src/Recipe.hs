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
import           Helpers                     (Action, TypedAction, runDB)

import           Network.HTTP.Types.Status   (created201)
import           Web.Scotty.Trans            (json, jsonData, status)

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
  deriving Show

Recipe json
  name Text
  description Text
  ingredients [IngredientRowId]
  created UTCTime Maybe default=now()
  deriving Show
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
insertIngredient :: Ingredient -> TypedAction (DB.Key Ingredient)
insertIngredient ingredient = runDB (DB.insert ingredient)

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
  _ <- insertRecipe recipe
  status created201
  json (newRecipe :: NewRecipe)

postIngredientA :: Action
postIngredientA = do
  ingredient <- jsonData
  time <- liftIO getCurrentTime
  let newIngredient = ingredient {ingredientCreated = Just time}
  _ <- insertIngredient newIngredient
  status created201
  json (newIngredient :: Ingredient)

-- Convert Api Models to DB Models
recipeFromNew :: NewRecipe -> [DB.Key IngredientRow] -> UTCTime -> Recipe
recipeFromNew newRecipe rows time =
  let name = newRecipeName newRecipe
      description = newRecipeName newRecipe
  in Recipe name description rows (Just time)

ingredientRowsFromNew :: NewRecipe -> UTCTime -> [IngredientRow]
ingredientRowsFromNew newRecipe time =
  let newIngredientRows = newRecipeIngredientsRow newRecipe
      ingredientRows =
        fmap
          (\i ->
             let rowAmount = newIngredientRowAmount i
                 rowUnit = newIngredientRowUnit i
                 rowIngredient = newIngredientRowIngredient i
             in IngredientRow rowAmount rowUnit rowIngredient (Just time))
          newIngredientRows
  in ingredientRows

getRecipesA :: Action
getRecipesA = do
  rs <- runDB (DB.selectList [] [DB.LimitTo 10])
  let rowIds = concatMap (\recipe -> recipeIngredients (DB.entityVal recipe)) rs
  rows <- runDB (DB.selectList [IngredientRowId DB.<-. rowIds] [])
  let ingredientIds = fmap (\row -> ingredientRowIngredient (DB.entityVal row)) rows
  ingredients <- runDB (DB.selectList [IngredientId DB.<-. ingredientIds] [])
  json (RecipeWithIngredients rs rows ingredients)

data RecipeWithIngredients = RecipeWithIngredients
  { recipes        :: [DB.Entity Recipe]
  , ingredientRows :: [DB.Entity IngredientRow]
  , ingredients    :: [DB.Entity Ingredient]
  } deriving (Generic, Show)

instance ToJSON RecipeWithIngredients
