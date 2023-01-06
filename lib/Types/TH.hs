{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Types.TH where

import           Data.Aeson
import           Data.Data
import           Data.Model
import           Data.Model.TH
import           GHC.Generics        (Generic)
import           Language.Haskell.TH

makeType ∷ Model → DecsQ
makeType model@Model { modelName, fields } = do
    qfields <- makeFieldTypes model
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
                            )) (addDefaultFields fields)
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
