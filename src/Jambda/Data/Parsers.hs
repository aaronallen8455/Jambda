module Jambda.Data.Parsers
  ( parseBeat
  , parseBpm
  , parseCell
  , parsePitch
  , parseOffset
  ) where

import            Control.Lens
import            Control.Monad (guard)
import            Control.Monad.Trans (lift)
import            Control.Monad.Trans.State (execStateT)
import            Data.Foldable (for_)
import            Data.Functor (($>))
import            Data.List.NonEmpty (NonEmpty(..))
import qualified  Data.List.NonEmpty as NonEmpty
import            Data.Semigroup (sconcat)
import            Data.Void (Void)
import            GHC.Exts (IsList(..))
import            Text.Megaparsec
import            Text.Megaparsec.Char

import            Jambda.Types

type Parser = Parsec Void String

parseBeat :: String -> Maybe (NonEmpty Cell)
parseBeat = parseMaybe ( beatP <* eof )

parseCell :: String -> Maybe Cell
parseCell = parseMaybe ( cellP ( > 0 ) )

parseOffset :: String -> Maybe Cell
parseOffset = parseMaybe ( cellP ( >= 0 ) )

parseBpm :: String -> Maybe BPM
parseBpm = parseMaybe bpmP

parsePitch :: String -> Maybe Pitch
parsePitch = parseMaybe pitchP

bpmP :: Parser BPM
bpmP = do
  v <- toRational <$> doubleP
  guard $ v > 0
  pure $ BPM v

doubleP :: Parser Double
doubleP = read <$> ( try mixedP <|> fmap ('0':) fracP )
  where
    numP   = some digitChar
    fracP  = (:) <$> char '.' <*> numP
    mixedP = do
      a <- numP
      b <- fmap (maybe "" id) $ optional fracP
      pure $ a ++ b

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

cellP :: (Double -> Bool) -> Parser Cell
cellP pred = do
  v <- expressionP
  guard $ pred v
  pure $ Cell v

repCellP :: Parser (NonEmpty Cell)
repCellP = nonEmptyGuard $ do
  cell <- cellP ( > 0 ) <* space
  guard $ cell > 0
  ( reps, ltm ) <- fmap ( maybe ( 1, 0 ) id ) . optional $ do
    reps <- between ( char '(' <* space )
                    ( char ')' <* space )
                    intP
    ltm <- maybe 0 id <$> optional expressionP <* space
    pure ( reps, Cell ltm )
  guard $ reps > 0 && cell + ltm > 0
  pure . reverse $ cell + ltm : replicate ( reps - 1 ) cell

blockRepP :: Parser (NonEmpty Cell)
blockRepP = do
    inner <- between ( char '[' <* space )
                     ( char ']' <* space )
                     beatP
    (reps, ltm) <- tagP <* space
    let ( lastCell :| rest ) = NonEmpty.reverse inner
    guard $ reps > 0 && lastCell + ltm > 0
    pure . sconcat . NonEmpty.reverse
          $ ( NonEmpty.reverse $ lastCell + ltm :| rest )
         :| ( replicate ( reps - 1 ) inner )
  where
    tagP = try simple <|> complex
    simple = (,) <$> intP <*> pure 0
    complex = do
      reps <- between ( char '(' <* space )
                      ( char ')' <* space )
                      intP
      ltm <- expressionP
      pure (reps, Cell ltm)

blockMultP :: Parser (NonEmpty Cell)
blockMultP = do
  inner <- between ( char '{' <* space )
                   ( char '}' <* space )
                   beatP
  factor <- Cell <$> expressionP
  guard $ factor > 0
  pure $ fmap ( * factor ) inner

beatP :: Parser (NonEmpty Cell)
beatP = fmap sconcat . nonEmptyGuard
      $ space *> (   try repCellP
                 <|> try blockRepP
                 <|> blockMultP
                 ) `sepBy1` ( char ',' <* space )

nonEmptyGuard :: Parser [a] -> Parser (NonEmpty a)
nonEmptyGuard p = do
  xs <- p
  guard . not $ null xs
  pure $ NonEmpty.fromList xs

pitchP :: Parser Pitch
pitchP = try ( Pitch <$> ( ANat  <$   string' "A"                     ) <*> octaveP )
     <|> try ( Pitch <$> ( BFlat <$ ( string' "Bb" <|> string' "A#" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( BNat  <$ ( string' "B"  <|> string' "Cb" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( CNat  <$ ( string' "C"  <|> string' "B#" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( DFlat <$ ( string' "C#" <|> string' "Db" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( DNat  <$   string' "D"                     ) <*> octaveP )
     <|> try ( Pitch <$> ( EFlat <$ ( string' "Eb" <|> string' "D#" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( ENat  <$ ( string' "E"  <|> string' "Fb" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( FNat  <$ ( string' "F"  <|> string' "E#" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( GFlat <$ ( string' "F#" <|> string' "Gb" ) ) <*> octaveP )
     <|> try ( Pitch <$> ( GNat  <$   string' "G"                     ) <*> octaveP )
     <|>     ( Pitch <$> ( AFlat <$ ( string' "G#" <|> string' "Ab" ) ) <*> octaveP )

octaveP :: Parser Octave
octaveP = do
  n <- intP
  guard $ n > 0 && n <= 12
  pure $ Octave n

intP :: Parser Int
intP = read <$> some digitChar
