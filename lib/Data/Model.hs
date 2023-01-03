module Data.Model where

import Language.Haskell.TH ( Name )

type ModelName = String
type FieldName = String
type Endpoint = String

data Field = Field {
    lowerField :: FieldName,
    upperField :: FieldName,
    typeName :: Name
}

type Fields = [Field]

data Model = Model {
    modelName       :: ModelName,
    pluralModelName :: ModelName,
    endpoint        :: Endpoint,
    pluralEndpoint  :: Endpoint,
    fields          :: Fields
}
