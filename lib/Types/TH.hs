{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Unsafe          #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types.TH where

import Data.Aeson
import Data.Aeson.Types           qualified as A
import Data.ByteString            (ByteString)
import Data.Data
import Data.List                  (find)
import Data.Model
import Data.Model.TH
import Data.Text.Encoding         qualified as TE
import GHC.Generics               (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Servant.API

-- for now let's just encodeUtf8 / decodeUtf8Lenient to present to json
-- and let's deal with others and newtypings later
instance FromJSON ByteString where
    parseJSON (String text) = pure (TE.encodeUtf8 text)
    parseJSON a             = A.typeMismatch "String" a

instance ToJSON ByteString where
    toJSON bs = String (TE.decodeUtf8Lenient bs)

instance FromHttpApiData ByteString where
    parseHeader = Right
    parseUrlPiece = Right . TE.encodeUtf8
    parseQueryParam = Right . TE.encodeUtf8

instance ToHttpApiData ByteString where
    toHeader = id
    toUrlPiece = TE.decodeUtf8Lenient
    toQueryParam = TE.decodeUtf8Lenient

makeJSONInstances ∷ Name → Fields → DecsQ
makeJSONInstances name fields = [d|
    instance FromJSON $(conT name) where
        parseJSON = genericParseJSON (defaultOptions {
            fieldLabelModifier = \jsonFieldName -> case find (\fieldFound -> jsonFieldName == lowerField fieldFound) $(lift fields) of
                Just field -> dbField field
                Nothing -> error $ "Unknown db field for lower field name: " <> jsonFieldName
        })
    instance ToJSON $(conT name) where
        toJSON = genericToJSON (defaultOptions {
            fieldLabelModifier = \jsonFieldName -> case find (\fieldFound -> jsonFieldName == lowerField fieldFound) $(lift fields) of
                Just field -> dbField field
                Nothing -> error $ "Unknown db field for lower field name: " <> jsonFieldName
        })
    |]

makeCreateType ∷ Model → DecsQ
makeCreateType model@Model { singularType, createFields = Just fields } = do
    let singularType' = mkName ("Create" <> singularType)
    qfields <- makeCreateFieldTypes model
    insts <- makeJSONInstances singularType' (addDefaultFieldsForCreate fields)
    pure $ qfields <> [
        do
            DataD
                []
                singularType'
                []
                Nothing
                [
                    RecC
                        singularType'
                        (
                            fmap (\Field { lowerField = lowerField, upperField = upperField } -> (
                                mkName lowerField,
                                Bang NoSourceUnpackedness NoSourceStrictness,
                                ConT (mkName ("Create" <> singularType <> upperField))
                            )) fields
                        )
                ]
                [
                    DerivClause
                        (Just StockStrategy)
                        [
                            ConT ''Show,
                            ConT ''Eq,
                            ConT ''Generic,
                            ConT ''Data
                        ]
                    -- DerivClause
                    --     (Just AnyclassStrategy)
                    --     [
                    --         ConT ''FromJSON,
                    --         ConT ''ToJSON
                    --     ]
                ]
        ] <> insts
makeCreateType Model { createFields = Nothing } = pure []


makeRetrieveType ∷ Model → DecsQ
makeRetrieveType model@Model { singularType, retrieveFields = Just fields } = do
    let singularType' = mkName singularType
    qfields <- makeRetrieveFieldTypes model
    insts <- makeJSONInstances singularType' (addDefaultFieldsForRetrieve fields)
    pure $ qfields <> [
        do
            DataD
                []
                singularType'
                []
                Nothing
                [
                    RecC
                        singularType'
                        (
                            fmap (\Field { lowerField = lowerField, upperField = upperField } -> (
                                mkName lowerField,
                                Bang NoSourceUnpackedness NoSourceStrictness,
                                ConT (mkName (singularType <> upperField))
                            )) (addDefaultFieldsForRetrieve fields)
                        )
                ]
                [
                    DerivClause
                        (Just StockStrategy)
                        [
                            ConT ''Show,
                            ConT ''Eq,
                            ConT ''Generic,
                            ConT ''Data
                        ]
                    -- DerivClause
                    --     (Just AnyclassStrategy)
                    --     [
                    --         ConT ''FromJSON,
                    --         ConT ''ToJSON
                    --     ]
                ]
        ] <> insts
makeRetrieveType Model { retrieveFields = Nothing } = pure []

makeUpdateType ∷ Model → DecsQ
makeUpdateType model@Model { singularType, updateFields = Just fields } = do
    let singularType' = mkName ("Update" <> singularType)
    qfields <- makeUpdateFieldTypes model
    insts <- makeJSONInstances singularType' (addDefaultFieldsForUpdate fields)
    pure $ qfields <> [
        do
            DataD
                []
                singularType'
                []
                Nothing
                [
                    RecC
                        singularType'
                        (
                            fmap (\Field { lowerField = lowerField, upperField = upperField } -> (
                                mkName lowerField,
                                Bang NoSourceUnpackedness NoSourceStrictness,
                                ConT (mkName ("Update" <> singularType <> upperField))
                            )) (addDefaultFieldsForUpdate fields)
                        )
                ]
                [
                    DerivClause
                        (Just StockStrategy)
                        [
                            ConT ''Show,
                            ConT ''Eq,
                            ConT ''Generic,
                            ConT ''Data
                        ]
                    -- DerivClause
                    --     (Just AnyclassStrategy)
                    --     [
                    --         ConT ''FromJSON,
                    --         ConT ''ToJSON
                    --     ]
                ]
        ] <> insts
makeUpdateType Model { updateFields = Nothing } = pure []

makeDeleteType ∷ Model → DecsQ
makeDeleteType model@Model { singularType, deleteFields = Just fields } = do
    let singularType' = mkName ("Delete" <> singularType)
    qfields <- makeDeleteFieldTypes model
    insts <- makeJSONInstances singularType' (addDefaultFieldsForDelete fields)
    pure $ qfields <> [
        do
            DataD
                []
                singularType'
                []
                Nothing
                [
                    RecC
                        singularType'
                        (
                            fmap (\Field { lowerField = lowerField, upperField = upperField } -> (
                                mkName lowerField,
                                Bang NoSourceUnpackedness NoSourceStrictness,
                                ConT (mkName ("Delete" <> singularType <> upperField))
                            )) (addDefaultFieldsForDelete fields)
                        )
                ]
                [
                    DerivClause
                        (Just StockStrategy)
                        [
                            ConT ''Show,
                            ConT ''Eq,
                            ConT ''Generic,
                            ConT ''Data
                        ]
                    -- DerivClause
                    --     (Just AnyclassStrategy)
                    --     [
                    --         ConT ''FromJSON,
                    --         ConT ''ToJSON
                    --     ]
                ]
        ] <> insts
makeDeleteType Model { deleteFields = Nothing } = pure []

makeCRUDTypes ∷ Model → DecsQ
makeCRUDTypes model = do
    createType <- makeCreateType model
    retrieveType <- makeRetrieveType model
    updateType <- makeUpdateType model
    deleteType <- makeDeleteType model
    pure $ createType <> retrieveType <> updateType <> deleteType
