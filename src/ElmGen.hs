{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ElmGen where

import           Elm (ElmType, toElmType)
import qualified Servant.Elm as SE
import Data.Int (Int64)

import Models.Hub
import Models.Tank
import Models.Reading
import Api.Hub
import Api.Tank
import Api.Reading
import App

int64ToInt :: Int64 -> Int
int64ToInt = fromIntegral

instance ElmType Int64 where
  toElmType = toElmType . int64ToInt

instance SE.ElmType HubRead
instance SE.ElmType HubWrite
instance SE.ElmType TankRead
instance SE.ElmType TankWrite
instance SE.ElmType ReadingRead
instance SE.ElmType ReadingWrite

hubSpec :: SE.Spec
hubSpec = SE.Spec ["Generated", "HubAPI"]
  (SE.defElmImports : SE.generateElmForAPI hubAPI)

tankSpec :: SE.Spec
tankSpec = SE.Spec ["Generated", "TankAPI"]
  (SE.defElmImports : SE.generateElmForAPI tankAPI)

readingSpec :: SE.Spec
readingSpec = SE.Spec ["Generated", "ReadingAPI"]
  (SE.defElmImports : SE.generateElmForAPI readingAPI)
