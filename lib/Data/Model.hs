{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE Unsafe #-}

module Data.Model where

import Data.Map            (Map)
import Data.Set            (Set)
import GHC.Stack           (HasCallStack)
import Language.Haskell.TH (Name)
import Language.Haskell.TH.Syntax (Lift)
import Types.UserType

type ModelName = String -- really? well mkName is required
type FieldName = String -- uhhh... make it easy
type Endpoint = String -- url piece

data DisplayOptions = DisplayCreatable | DisplayEditable | DisplayEditableInline

data TableDisplayOption = HideFromTable | ShowInTable | EditInTable

data CRUDPermission = Create | Retrieve | Update | Delete

data AuthState = Unauthenticated | Authenticated UserType

newtype CRUDPermissions = CRUDPermissions (Map AuthState (Set CRUDPermission))

unauthenticatedPermissions ∷ CRUDPermissions
unauthenticatedPermissions = undefined

normalPermissions ∷ CRUDPermissions
normalPermissions = undefined

adminPermissions ∷ CRUDPermissions
adminPermissions = undefined

superuserPermissions ∷ CRUDPermissions
superuserPermissions = undefined

data FilterOptions = FilterNone | FilterEq | FilterOrd
    deriving stock (Eq, Ord, Lift)

data OrderOptions = NotOrderable | Orderable
    deriving stock (Eq, Ord, Lift)

data Field = Field {
    lowerField :: FieldName,
    upperField :: FieldName,
    typeName   :: Name,
    dbField   :: FieldName,
    filterOptions :: FilterOptions,
    orderOptions :: OrderOptions
    -- humanName  :: String,
    -- showInCreate :: Bool,
    -- showInEdit :: Bool,
    -- showInTable :: TableDisplayOption
} deriving stock Lift

defaultField ∷ HasCallStack ⇒ Field
defaultField = Field {
    lowerField = error "Required: lower field",
    upperField = error "Required: upper field",
    typeName = error "Required: type name",
    dbField = error "Required: db field",
    filterOptions = FilterNone,
    orderOptions = NotOrderable
}

type Fields = [Field]

data Model = Model {
    singularType     :: ModelName,
    pluralType       :: ModelName,
    singularEndpoint :: Endpoint,
    pluralEndpoint   :: Endpoint,
    -- we may want to put these under another field and use defaults and function values like "onlyId = ..." or "disallowed = ..." but for now that's it
    createFields     :: Maybe Fields, -- if Nothing, operation not supported -  better than per field at least because you can ban the endpoint entirely?
    retrieveFields   :: Maybe Fields, -- if Nothing, operation not supported -  better than per field at least because you can ban the endpoint entirely?
    updateFields     :: Maybe Fields, -- if Nothing, operation not supported -  better than per field at least because you can ban the endpoint entirely?
    deleteFields     :: Maybe Fields -- if Nothing, operation not supported -  better than per field at least because you can ban the endpoint entirely?
    -- crudPermissions :: CRUDPermissions
}

defaultModel ∷ HasCallStack ⇒ Model
defaultModel = Model {
    singularType = error "Required: model name",
    pluralType = error "Required: plural model name",
    singularEndpoint = error "Required: singularEndpoint",
    pluralEndpoint = error "Required: pluralEndpoint",
    createFields = error "Required: createFields",
    retrieveFields = error "Required: retrieveFields",
    updateFields = error "Required: updateFields",
    deleteFields = error "Required: deleteFields"
}

isFilterable :: Field -> Bool
isFilterable Field { filterOptions = FilterEq } = True
isFilterable Field { filterOptions = FilterOrd } = True
isFilterable _ = False

-- TODO lowerField!!!
getFilterableFieldNames :: Model -> [String]
getFilterableFieldNames Model { retrieveFields = Just retrieveFields' } = dbField <$> filter isFilterable retrieveFields'
getFilterableFieldNames _ = []