{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Unsafe                #-}

module Data.Model.TH where

import Data.Aeson
import Data.Data
import Data.Model
import Data.Text (Text)
import Data.Time
import Data.UUID.Types
import Language.Haskell.TH
import Servant.API

type MaybeUTCTime = Maybe UTCTime
type MaybeText = Maybe Text
type MaybeUUID = Maybe UUID

-- Add models for CreateEntity, RetrieveEntity, UpdateEntity, DeleteEntityId
-- hkd?
-- CreateEntity has no ID or fields. (hkd partial replace Const Nothing?) - doesn't need ToHttpApiData
-- RetrieveEntity is the "normal" one except with extra fields!
-- UpdateEntity has everything Maybe'd except Id - doesn't need ToHttpApiData
-- DeleteEntity is a where clause so everything Maybe'd - doesn't need ToHttpApiData

-- nothing, just added for consistency for now
addDefaultFieldsForCreate ∷ Fields → Fields
addDefaultFieldsForCreate fields = fields

addDefaultFieldsForRetrieve ∷ Fields → Fields
addDefaultFieldsForRetrieve fields = [
        defaultField { lowerField = "id", upperField = "Id", dbField = "id", typeName = ''UUID }
    ] <> fields <> [
        -- we don't always want these, they are normally hidden
        -- defaultField { lowerField = "createdBy", upperField = "CreatedBy", dbField = "created_by", typeName = ''MaybeUUID },
        -- defaultField { lowerField = "createdAt", upperField = "CreatedAt", dbField = "created_at", typeName = ''Text }, -- Nothing for insertion
        -- defaultField { lowerField = "updatedAt", upperField = "UpdatedAt", dbField = "updated_at", typeName = ''MaybeText }
        -- defaultField { lowerField = "deletedAt", upperField = "DeletedAt", dbField = "deleted_at", typeName = ''MaybeUTCTime } -- @TODO admin see / undelete later
    ]

addDefaultFieldsForUpdate ∷ Fields → Fields
addDefaultFieldsForUpdate fields = [
        defaultField { lowerField = "id", upperField = "Id", dbField = "id", typeName = ''UUID }
    ] <> fields

addDefaultFieldsForDelete ∷ Fields → Fields
addDefaultFieldsForDelete fields = [
        defaultField { lowerField = "id", upperField = "Id", dbField = "id", typeName = ''UUID }
    ] <> fields

makeCreateFieldTypes ∷ Model → Q [Dec]
makeCreateFieldTypes Model { singularType, createFields = Just fields } = traverse (\Field { upperField = upperField, typeName = typeName } -> do
    let fieldName = mkName ("Create" <> singularType <> upperField)
    let getName = mkName ("getCreate" <> singularType <> upperField)
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
                    ConT ''Ord,
                    ConT ''Eq
                ],
            DerivClause
                (Just StockStrategy)
                [
                    ConT ''Data
                ]
        ]
    ) (addDefaultFieldsForCreate fields)
makeCreateFieldTypes Model { createFields = Nothing } = pure []

makeRetrieveFieldTypes ∷ Model → Q [Dec]
makeRetrieveFieldTypes Model { singularType, retrieveFields = Just fields } = traverse (\Field { upperField = upperField, typeName = typeName } -> do
    let fieldName = mkName (singularType <> upperField)
    let getName = mkName ("get" <> singularType <> upperField)
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
                    ConT ''Ord,
                    ConT ''Eq
                ],
            DerivClause
                (Just StockStrategy)
                [
                    ConT ''Data
                ]
        ]
    ) (addDefaultFieldsForRetrieve fields)
makeRetrieveFieldTypes Model { retrieveFields = Nothing } = pure []


makeUpdateFieldTypes ∷ Model → Q [Dec]
makeUpdateFieldTypes Model { singularType, updateFields = Just fields } = traverse (\Field { upperField = upperField, typeName = typeName } -> do
    let fieldName = mkName ("Update" <> singularType <> upperField)
    let getName = mkName ("getUpdate" <> singularType <> upperField)
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
                    ConT ''Ord,
                    ConT ''Eq
                ],
            DerivClause
                (Just StockStrategy)
                [
                    ConT ''Data
                ]
        ]
    ) (addDefaultFieldsForUpdate fields)
makeUpdateFieldTypes Model { updateFields = Nothing } = pure []


makeDeleteFieldTypes ∷ Model → Q [Dec]
makeDeleteFieldTypes Model { singularType, deleteFields = Just fields } = traverse (\Field { upperField = upperField, typeName = typeName } -> do
    let fieldName = mkName ("Delete" <> singularType <> upperField)
    let getName = mkName ("getDelete" <> singularType <> upperField)
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
                    ConT ''Ord,
                    ConT ''Eq
                ],
            DerivClause
                (Just StockStrategy)
                [
                    ConT ''Data
                ]
        ]
    ) (addDefaultFieldsForDelete fields)
makeDeleteFieldTypes Model { deleteFields = Nothing } = pure []