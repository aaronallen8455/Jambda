module Jambda.Data.Parsers
  ( parseBeat
  , parseBpm
  , parseCell
  ) where

import Data.Functor (($>))

import Control.Lens
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State
import Data.Foldable (for_)
import Data.Void (Void)
import GHC.Exts (IsList(..))
import Text.Megaparsec
import Text.Megaparsec.Char

import Jambda.Types

type Parser = Parsec Void String

parseBeat :: String -> Maybe [Cell]
parseBeat = parseMaybe beatP

parseCell :: String -> Maybe Cell
parseCell = parseMaybe cellP

parseBpm :: String -> Maybe BPM
parseBpm = parseMaybe bpmP

bpmP :: Parser BPM
bpmP = do
  v <- doubleP
  guard $ v > 0
  pure $ BPM v

doubleP :: Parser Double
doubleP = do
  n <- try ( char '-' ) *> pure negate <|> pure id
  w <- many digitChar
  d <- maybe "0" id <$> optional ( char '.' *> some digitChar )
  pure . n . read $ w ++ '.' : d

data Operator
  = Add
  | Sub
  | Mul
  | Div

operatorP :: Parser Operator
operatorP = char '+' $> Add
        <|> char '-' $> Sub
        <|> char '*' $> Mul
        <|> char '/' $> Div

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
                then failure ( Just . Tokens $ fromList "divide by 0" ) mempty
                else mult ( 1 / num )

cellP :: Parser Cell
cellP = do
  v <- expressionP
  guard $ v > 0
  pure $ Cell v

beatP :: Parser [Cell]
beatP = space *> cellP `sepBy` ( char ',' <* space )
