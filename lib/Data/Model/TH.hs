{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Model.TH where

import Data.Aeson
import Data.Data
import Data.Model
import Data.Time
import Data.UUID.Types
import Language.Haskell.TH
import Servant.API

type MaybeUTCTime = Maybe UTCTime

addDefaultFields ∷ Fields → Fields
addDefaultFields fields = [
        Field { lowerField = "id", upperField = "Id", typeName = ''UUID }
    ] <> fields <> [
        Field { lowerField = "createdAt", upperField = "CreatedAt", typeName = ''UTCTime },
        Field { lowerField = "updatedAt", upperField = "UpdatedAt", typeName = ''MaybeUTCTime },
        Field { lowerField = "deletedAt", upperField = "DeletedAt", typeName = ''MaybeUTCTime }
    ]

makeFieldTypes ∷ Model → Q [Dec]
makeFieldTypes Model { modelName, fields } = mapM (\Field { upperField = upperField, typeName = typeName } -> do
    let fieldName = mkName (modelName <> upperField)
    let getName = mkName ("get" <> modelName <> upperField)
    pure $ NewtypeD
        []
        fieldName
        []
        Nothing
        (
            RecC fieldName [
                (
                    getName,
                    Bang NoSourceUnpackedness NoSourceStrictness,
                    ConT typeName
                )
            ]
        )
        [
            DerivClause
                (
                    Just (
                        ViaStrategy (
                            ConT typeName
                        )
                    )
                )
                [
                    ConT ''FromJSON,
                    ConT ''ToJSON,
                    ConT ''FromHttpApiData,
                    ConT ''ToHttpApiData,
                    ConT ''Show,
                    ConT ''Eq
                ],
            DerivClause
                (Just StockStrategy)
                [
                    ConT ''Data
                ]
        ]
    ) (addDefaultFields fields)
