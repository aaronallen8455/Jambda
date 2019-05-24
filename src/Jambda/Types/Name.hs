module Jambda.Types.Name
  ( Name (..)
  , LayerFieldName (..)
  ) where

data Name
  = LayerName !Int !LayerFieldName
  | TempoName
  | PlayName
  | StopName
  | AddLayerName
  | Viewport
  | MasterVolumeName
  deriving (Eq, Ord, Show)

data LayerFieldName
  = BeatCodeName
  | OffsetName
  | NoteName
  | DeleteName
  deriving (Eq, Ord, Show)
