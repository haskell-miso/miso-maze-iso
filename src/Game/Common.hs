{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Common where

import Control.Monad
import Control.Applicative
import Data.Aeson as Aeson
import Data.Char
import Data.Vector qualified as V
import GHC.Generics
import Miso
import Miso.Lens
import Miso.Lens.TH
import Miso.Util.Lexer as L
import Miso.Util.Parser as P

-------------------------------------------------------------------------------
-- basic types
-------------------------------------------------------------------------------

data Position = Position
  { _posI :: Int
  , _posJ :: Int
  } deriving (Eq, Generic, Show)

makeLenses ''Position

instance ToJSON Position where
    toEncoding = genericToEncoding Aeson.defaultOptions

instance FromJSON Position

data Player = Player
  { _playerName :: MisoString
  , _playerCol :: Int
  , _playerPos :: Position
  } deriving (Eq, Generic, Show)

makeLenses ''Player

instance ToJSON Player where
    toEncoding = genericToEncoding Aeson.defaultOptions

instance FromJSON Player

data Move
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  deriving (Eq, Show, Generic)

instance ToJSON Move where
    toEncoding = genericToEncoding Aeson.defaultOptions

instance FromJSON Move

-------------------------------------------------------------------------------
-- Board
-------------------------------------------------------------------------------

data Cell
  = CellEmpty
  | CellWall
  deriving (Eq, Show)

data Board = Board
  { _boardNi :: Int
  , _boardNj :: Int
  , _boardData :: V.Vector Cell
  } deriving (Eq, Show)

makeLenses ''Board

forBoard :: (Monad m) => Board -> (Int -> Int -> Cell -> m ()) -> m ()
forBoard Board{..} f = 
  V.iforM_ _boardData $ \k c -> 
    let (i, j) = k2ij _boardNj k
    in f i j c

ij2k :: Int -> (Int, Int) -> Int
ij2k nj (i, j) = i*nj + j

k2ij :: Int -> Int -> (Int, Int)
k2ij nj k = (k `div` nj, k`rem` nj)

data Token
  = TokNb Int
  | TokWall
  | TokEmpty
  | TokSpace
  deriving (Eq, Show)

lexerInt :: Lexer Int
lexerInt = read <$> some (L.satisfy isDigit)

lexerToken :: Lexer Token
lexerToken
  =   lexerNb
  <|> lexerWall
  <|> lexerEmpty
  <|> lexerSpace
  where
    lexerNb = TokNb <$> lexerInt
    lexerWall = char 'x' >> pure TokWall
    lexerEmpty = char '.' >> pure TokEmpty 
    lexerSpace = (char ' ' <|> char '\n') >> pure TokSpace

lexerTokens :: Lexer [Token]
lexerTokens = filter (/=TokSpace) <$> some lexerToken

parserBoard :: Parser Token Board
parserBoard = do
  ni <- parseNb
  nj <- parseNb
  cells <- parseCells
  guard (ni*nj == V.length cells)
  pure $ Board ni nj cells

  where

    isTokNb = \case
      TokNb _ -> True
      _       -> False

    tokNb = \case
      TokNb n -> n
      _ -> error "not a TokNb"

    parseNb = tokNb <$> P.satisfy isTokNb 

    parseWall = P.satisfy (==TokWall) >> pure CellWall 

    parseEmpty = P.satisfy (==TokEmpty) >> pure CellEmpty 

    parseCells = V.fromList <$> some (parseWall <|> parseEmpty)

parseBoard :: MisoString -> Maybe Board
parseBoard str = 
  case runLexer lexerTokens (mkStream str) of
    Left _ -> Nothing
    Right (tokens, _) -> either (const Nothing) Just $ parse parserBoard tokens

