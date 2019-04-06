module Jambda.UI.Layer
  ( modifyBeat
  , resetLayer
  ) where

import Control.Lens

import Jambda.Types
import Jambda.Data (parseBeat, silence)

-- | Change the beatcode of a Layer
modifyBeat :: Cell -> String -> Layer -> Maybe Layer
modifyBeat elapsedCells beatCode layer = do
  cells <- parseBeat beatCode

-- TODO what if elapsedCells is less than the offset?
-- TODO the partial amount is not being accounted?
  let cycleSize = sum cells
      elapsedCycles = elapsedCells / cycleSize
      wholeCycles = fromIntegral $ truncate elapsedCycles
      cellsToDrop = elapsedCells - wholeCycles * cycleSize -- partialCycles * cycleSize
      cellCycle = cycle cells
      (cellPrefix, newCells) = dropCells cellsToDrop cellCycle
      dropCells dc (c:cs)
        | c >= dc = (c - dc, cs)
        | otherwise = dropCells (dc - c) cs

  pure $
    layer & layerBeat         .~ newCells
          & layerCode         .~ beatCode
          & layerCellPrefix   .~ cellPrefix
          & layerParsedCode   .~ cells

-- | Reset a layer to it's initial state
resetLayer :: Layer -> Layer
resetLayer layer =
  layer & layerBeat .~ ( cycle $ layer^.layerParsedCode )
        & layerCellPrefix .~ ( layer^.layerCellOffset )
        & layerSourcePrefix .~ []
