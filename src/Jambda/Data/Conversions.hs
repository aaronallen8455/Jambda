module Jambda.Data.Conversions
  ( numSamplesForCellValue
  , numSamplesToCellValue
  , cellValueToSecs
  , secToCellValue
  , numSampsToSecs
  , secToNumSamps
  ) where

import Jambda.Types
import Jambda.Data.Constants (sampleRate)

-- | Get the number of whole samples from a cell value and also
-- return the cell value corresponding to the leftover fractional sample
numSamplesForCellValue :: BPM -> CellValue -> (Int, CellValue)
numSamplesForCellValue bpm cell = (wholeSamps, remCell) where
  samples = secToNumSamps $ cellValueToSecs bpm cell
  wholeSamps = floor samples
  remCell = numSamplesToCellValue bpm $ samples - fromIntegral wholeSamps

numSamplesToCellValue :: BPM -> Double -> CellValue
numSamplesToCellValue bpm = secToCellValue bpm . numSampsToSecs

cellValueToSecs :: BPM -> CellValue -> Sec
cellValueToSecs (BPM bpm) (CellValue cell) = Sec $
  ( 1 / ( bpm / 60 ) ) * cell

secToCellValue :: BPM -> Sec -> CellValue
secToCellValue (BPM bpm) (Sec sec) = CellValue $ ( bpm / 60 ) * sec

numSampsToSecs :: Double -> Sec
numSampsToSecs nsamps = Sec $
  nsamps / sampleRate

secToNumSamps :: Sec -> Double
secToNumSamps (Sec sec) = sec * sampleRate
