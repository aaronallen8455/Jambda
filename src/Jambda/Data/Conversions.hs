module Jambda.Data.Conversions
  ( numSamplesForCell
  , numSamplesToCells
  , cellToSecs
  , secToCells
  , numSampsToSecs
  , secToNumSamps
  ) where

import Jambda.Types
import Jambda.Data.Constants (sampleRate)

-- | Get the number of whole samples from a cell value and also
-- return the cell value corresponding to the leftover fractional sample
numSamplesForCell :: BPM -> Cell -> (Int, Cell)
numSamplesForCell bpm cell = (wholeSamps, remCell) where
  samples = secToNumSamps $ cellToSecs bpm cell
  wholeSamps = floor samples
  remCell = numSamplesToCells bpm $ samples - fromIntegral wholeSamps

numSamplesToCells :: BPM -> Double -> Cell
numSamplesToCells bpm = secToCells bpm . numSampsToSecs

cellToSecs :: BPM -> Cell -> Sec
cellToSecs (BPM bpm) (Cell cell) = Sec $
  1 / (bpm / 60) * cell

secToCells :: BPM -> Sec -> Cell
secToCells (BPM bpm) (Sec sec) = Cell $ bpm / 60 * sec

numSampsToSecs :: Double -> Sec
numSampsToSecs nsamps = Sec $
  nsamps / sampleRate

secToNumSamps :: Sec -> Double
secToNumSamps (Sec sec) = sec * sampleRate
