module Core where

-----------------------------------------------------------------------------------------
-- Core Data
-----------------------------------------------------------------------------------------
newtype TypeId = MkTypeId Word16
  deriving (Generic,Eq,Show)

instance From TypeId Int where
  from (MkTypeId w) = from w

instance TryFrom Int TypeId where
  tryFrom i = case tryFrom i of
    Left tfe -> Left $ coerce tfe
    Right a  -> Right $ MkTypeId a

derivingUnbox "TypeId" [t|TypeId -> Word16|] [|\(MkTypeId w) -> w|] [|MkTypeId|]

instance Hashable TypeId

-----------------------------------------------------------------------------------------
newtype ArchId = MkArchId Word32
  deriving (Generic,Eq,Show)

instance From ArchId Int where
  from (MkArchId a) = fromIntegral a

derivingUnbox "ArchId" [t|ArchId -> Word32|] [|\(MkArchId i) -> i|] [|MkArchId|]

instance Hashable ArchId
-----------------------------------------------------------------------------------------

newtype EntityId = MkEntityId Word32
  deriving (Generic,Eq,Show)

instance From EntityId Int where
  from (MkEntityId a) = fromIntegral a

derivingUnbox "EntityId" [t|EntityId -> Word32|] [|\(MkEntityId i) -> i|] [|MkEntityId|]

instance Hashable EntityId

-----------------------------------------------------------------------------------------
