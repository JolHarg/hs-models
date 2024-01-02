{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Unsafe #-}

module Types.TH where

import Data.Aeson
import Data.Data
import Data.Model
import Data.Model.TH
import GHC.Generics        (Generic)
import Language.Haskell.TH

makeCreateType ∷ Model → DecsQ
makeCreateType model@Model { modelName, fields } = do
    qfields <- makeCreateFieldTypes model
    pure $ qfields <> [
        do
            let modelName' = mkName ("Create" <> modelName)
            DataD
                []
                modelName'
                []
                Nothing
                [
                    RecC
                        modelName'
                        (
                            fmap (\Field { lowerField = lowerField, upperField = upperField } -> (
                                mkName lowerField,
                                Bang NoSourceUnpackedness NoSourceStrictness,
                                ConT (mkName ("Create" <> modelName <> upperField))
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
                        ],
                    DerivClause
                        (Just AnyclassStrategy)
                        [
                            ConT ''FromJSON,
                            ConT ''ToJSON
                        ]
                ]
        ]


makeRetrieveType ∷ Model → DecsQ
makeRetrieveType model@Model { modelName, fields, extraViewFields } = do
    qfields <- makeRetrieveFieldTypes model
    pure $ qfields <> [
        do
            let modelName' = mkName modelName
            DataD
                []
                modelName'
                []
                Nothing
                [
                    RecC
                        modelName'
                        (
                            fmap (\Field { lowerField = lowerField, upperField = upperField } -> (
                                mkName lowerField,
                                Bang NoSourceUnpackedness NoSourceStrictness,
                                ConT (mkName (modelName <> upperField))
                            )) (addDefaultFieldsForRetrieve (fields <> extraViewFields))
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
                        ],
                    DerivClause
                        (Just AnyclassStrategy)
                        [
                            ConT ''FromJSON,
                            ConT ''ToJSON
                        ]
                ]
        ]


makeUpdateType ∷ Model → DecsQ
makeUpdateType model@Model { modelName, fields } = do
    qfields <- makeUpdateFieldTypes model
    pure $ qfields <> [
        do
            let modelName' = mkName ("Update" <> modelName)
            DataD
                []
                modelName'
                []
                Nothing
                [
                    RecC
                        modelName'
                        (
                            fmap (\Field { lowerField = lowerField, upperField = upperField } -> (
                                mkName lowerField,
                                Bang NoSourceUnpackedness NoSourceStrictness,
                                ConT (mkName ("Update" <> modelName <> upperField))
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
                        ],
                    DerivClause
                        (Just AnyclassStrategy)
                        [
                            ConT ''FromJSON,
                            ConT ''ToJSON
                        ]
                ]
        ]

makeDeleteType ∷ Model → DecsQ
makeDeleteType model@Model { modelName, fields } = do
    qfields <- makeDeleteFieldTypes model
    pure $ qfields <> [
        do
            let modelName' = mkName ("Delete" <> modelName)
            DataD
                []
                modelName'
                []
                Nothing
                [
                    RecC
                        modelName'
                        (
                            fmap (\Field { lowerField = lowerField, upperField = upperField } -> (
                                mkName lowerField,
                                Bang NoSourceUnpackedness NoSourceStrictness,
                                ConT (mkName ("Delete" <> modelName <> upperField))
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
                        ],
                    DerivClause
                        (Just AnyclassStrategy)
                        [
                            ConT ''FromJSON,
                            ConT ''ToJSON
                        ]
                ]
        ]

makeCRUDTypes :: Model -> DecsQ
makeCRUDTypes model = do
    createType <- makeCreateType model
    retrieveType <- makeRetrieveType model
    updateType <- makeUpdateType model
    deleteType <- makeDeleteType model
    pure $ createType <> retrieveType <> updateType <> deleteType