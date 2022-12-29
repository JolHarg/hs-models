module Data.Model where

import           Language.Haskell.TH

type ModelName = String
type FieldName = String
type Endpoint = String

type Field = (FieldName, FieldName, Name)

type Fields = [Field]

data Model = Model {
    modelName       :: ModelName,
    pluralModelName :: ModelName,
    endpoint        :: Endpoint,
    pluralEndpoint  :: Endpoint,
    fields          :: Fields
}
