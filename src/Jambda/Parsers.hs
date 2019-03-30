module Jambda.Parsers
  ( parseBeat
  , parseBpm
  , expressionP
  , doubleP
  , beatP
  ) where

import Control.Lens
import Control.Monad (guard)
import Control.Monad.Trans.State
import Control.Monad.Trans (lift)
import Data.Foldable (for_)
import Data.Void (Void)
import GHC.Exts (IsList(..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Jambda.Types (Layer(..))
import Jambda.Newtypes (BPM(..), Cell(..))

type Parser = Parsec Void String

parseBeat :: String -> Maybe [Cell]
parseBeat = parseMaybe beatP

parseBpm :: String -> Maybe BPM
parseBpm = parseMaybe bpmP

bpmP :: Parser BPM
bpmP = do
  v <- doubleP
  guard $ v > 0
  pure $ BPM v

doubleP :: Parser Double
doubleP = do
  n <- try (char '-') *> pure negate <|> pure id
  w <- many digitChar
  d <- withRecovery (const $ pure "0") ( char '.' *> some digitChar )
  pure . n . read $ w ++ '.' : d

data Operator
  = Add
  | Sub
  | Mul
  | Div

operatorP :: Parser Operator
operatorP = char '+' *> pure Add
        <|> char '-' *> pure Sub
        <|> char '*' *> pure Mul
        <|> char '/' *> pure Div

expressionP :: Parser Double
expressionP = fmap ( uncurry (+) )
            . ( `execStateT` ( 0, 0 ) ) $ do

  _2 <~ lift ( doubleP <* space )

  terms <- lift . many $ (,) <$> ( operatorP <* space )
                             <*> ( doubleP   <* space )

  let add n = do p  <- use _2
                 _1 += p
                 _2 .= n

      mult n = _2 *= n

  for_ terms $ \( operator, num ) ->
    case operator of
      Add -> add num
      Sub -> add $ negate num
      Mul -> mult num
      Div -> if num == 0
                then failure (Just . Tokens $ fromList "divide by 0") mempty
                else mult ( 1 / num )

cellP :: Parser Cell
cellP = Cell <$> expressionP

beatP :: Parser [Cell]
beatP = space *> cellP `sepBy` ( char ',' <* space )

