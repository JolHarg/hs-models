module Data.Model where

import Data.Map            (Map)
import Data.Set            (Set)
import Language.Haskell.TH (Name)
import Types.UserType

type ModelName = String
type FieldName = String
type Endpoint = String

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

data Field = Field {
    lowerField :: FieldName,
    upperField :: FieldName,
    typeName   :: Name
    -- humanName  :: String,
    -- showInCreate :: Bool,
    -- showInEdit :: Bool,
    -- showInTable :: TableDisplayOption
}

type Fields = [Field]

data Model = Model {
    modelName       :: ModelName,
    pluralModelName :: ModelName,
    endpoint        :: Endpoint,
    pluralEndpoint  :: Endpoint,
    fields          :: Fields,
    extraJoinFields :: Fields
    -- crudPermissions :: CRUDPermissions
}

defaultModel ∷ Model
defaultModel = Model {
    modelName = error "Required: model name",
    pluralModelName = error "Required: plural model name",
    endpoint = error "Required: endpoint",
    pluralEndpoint = error "Required: plural model endpoint",
    fields = error "Required: fields",
    extraJoinFields = []
}
