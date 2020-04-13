module Connect4.Board
  ( Board,
    empty,
    play,
    isWonGame,
    isTiedGame,
    legalMoves,
    isLegalMove,
  )
where

import           Connect4.Types
import           Data.Array.Unboxed
import           Data.Bits
import           Data.Word

width, height :: Int
(width, height) = (7, 6)

data Board = Board
    { numMoves   :: Int
    , toMove     :: Word64
    , nextToMove :: Word64
    , heights    :: UArray Int Int
    }
    deriving (Eq, Show)

empty :: Board
empty = Board
  { numMoves = 0,
    toMove = 0,
    nextToMove = 0,
    heights = listArray (0, width -1) [0, (height + 1) ..]
  }

isLegalMove :: Column -> Board -> Bool
isLegalMove (Column col) b = col `elem` legalMoves b

play :: Column -> Board -> Board
play (Column col) b = move b col

bottom :: Word64
bottom = (bit (width * (height + 1)) - 1) `div` (bit (height + 1) - 1)

top :: Word64
top = bottom `shiftL` height

allMoves :: [Int]
allMoves = [0 .. (width - 1)]

isLegal :: Word64 -> Bool
isLegal bitBoard = (bitBoard .&. top) == 0

isLegalGame :: Board -> Bool
isLegalGame = isLegal . nextToMove

move :: Board -> Int -> Board
move (Board n tm ntm hs) i =
  let h = hs ! i
   in Board (n + 1) ntm (setBit tm h) (hs // [(i, h + 1)])

legalMoves :: Board -> [Int]
legalMoves board = filter (isLegalGame . move board) allMoves

isWon :: Word64 -> Bool
isWon bitBoard = any ((/= 0) . dir) [1, height, height + 1, height + 2]
  where
    dir d = let t = bitBoard .&. (bitBoard `shiftR` d) in t .&. (t `shiftR` (2 * d))

isWonGame :: Board -> Bool
isWonGame = isWon . nextToMove

isTiedGame :: Board -> Bool
isTiedGame = null . legalMoves
