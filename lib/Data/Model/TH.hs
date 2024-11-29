{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Unsafe                #-}

module Data.Model.TH where

import Data.Aeson
import Data.Data
import Data.Model
import Data.Time
import Data.Traversable
import Data.UUID.Types
import Language.Haskell.TH
import Servant.API

type MaybeUTCTime = Maybe UTCTime
type MaybeUUID = Maybe UUID

-- Add models for CreateEntity, RetrieveEntity, UpdateEntity, DeleteEntityId
-- hkd?
-- CreateEntity has no ID or fields. (hkd partial replace Const Nothing?) - doesn't need ToHttpApiData
-- RetrieveEntity is the "normal" one except with extra fields!
-- UpdateEntity has everything Maybe'd except Id - doesn't need ToHttpApiData
-- DeleteEntity is a where clause so everything Maybe'd - doesn't need ToHttpApiData
addDefaultFieldsForRetrieve ∷ Fields → Fields
addDefaultFieldsForRetrieve fields = [
        defaultField { lowerField = "id", upperField = "Id", typeName = ''UUID }
    ] <> fields <> [
        defaultField { lowerField = "createdAt", upperField = "CreatedAt", typeName = ''UTCTime }, -- Nothing for insertion
        defaultField { lowerField = "updatedAt", upperField = "UpdatedAt", typeName = ''MaybeUTCTime }
        -- defaultField { lowerField = "deletedAt", upperField = "DeletedAt", typeName = ''MaybeUTCTime } -- @TODO admin see / undelete later
    ]

addDefaultFieldsForUpdate ∷ Fields → Fields
addDefaultFieldsForUpdate fields = [
        defaultField { lowerField = "id", upperField = "Id", typeName = ''UUID }
    ] <> fields

addDefaultFieldsForDelete ∷ Fields → Fields
addDefaultFieldsForDelete fields = [
        defaultField { lowerField = "id", upperField = "Id", typeName = ''UUID }
    ] <> fields

makeCreateFieldTypes ∷ Model → Q [Dec]
makeCreateFieldTypes Model { modelName, fields } = traverse (\Field { upperField = upperField, typeName = typeName } -> do
    let fieldName = mkName ("Create" <> modelName <> upperField)
    let getName = mkName ("getCreate" <> modelName <> upperField)
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
    ) fields

makeRetrieveFieldTypes ∷ Model → Q [Dec]
makeRetrieveFieldTypes Model { modelName, fields, extraViewFields } = traverse (\Field { upperField = upperField, typeName = typeName } -> do
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
    ) (addDefaultFieldsForRetrieve (fields <> extraViewFields))


makeUpdateFieldTypes ∷ Model → Q [Dec]
makeUpdateFieldTypes Model { modelName, fields } = traverse (\Field { upperField = upperField, typeName = typeName } -> do
    let fieldName = mkName ("Update" <> modelName <> upperField)
    let getName = mkName ("getUpdate" <> modelName <> upperField)
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
    ) (addDefaultFieldsForUpdate fields)


makeDeleteFieldTypes ∷ Model → Q [Dec]
makeDeleteFieldTypes Model { modelName, fields } = traverse (\Field { upperField = upperField, typeName = typeName } -> do
    let fieldName = mkName ("Delete" <> modelName <> upperField)
    let getName = mkName ("getDelete" <> modelName <> upperField)
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
    ) (addDefaultFieldsForDelete fields)
