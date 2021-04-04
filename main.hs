import Data.Map as Map

data Row       =  One   | Two  | Three 
                | Four  | Five | Six
                | Seven | Eight
               deriving 
                  (Show, Eq, Ord,
                   Enum, Bounded)
data Column    =   A | B | C | D
                 | E | F | G | H
                 deriving 
                    (Show, Eq, Ord, 
                    Enum, Bounded)
type Position  = (Row, Column)

data Dir       =  North | East 
                | West  | South
                | NE    | NW 
                | SE    | SW 
                deriving Show 

data Color     = Black | White 
                deriving (Show, Eq)
data PieceType =  King   | Queen 
                | Bishop | Rook 
                deriving (Show, Eq)
data Piece     = Piece { 
                pieceType :: PieceType,
                color:: Color
                        } 
                 deriving (Show, Eq)
type Board     = Map Position Piece
data Status    =  InPlay 
                | Check 
                | Checkmate 
                deriving (Show, Eq)
data GameState = GameState { 
                board :: Board, 
                toPlay :: Color 
                }
data MvLimit   = OneSquare | Unlimited
data Sides     = Sides { 
      toPlaySide :: Map Position Piece,
      opponents  :: Map Position Piece
                        } 
                  deriving Show
data AttackArgs = AttackArgs {
       fromPos :: Position,
       toPos   :: Position,
       attackers :: Map Position Piece,
       defenders :: Map Position Piece
                             }
                  deriving Show

data Occupancy = Empty 
               | Defender
               | Attacker
                  deriving (Show, Eq)
    
determineStatus :: GameState -> Status
determineStatus gs | kingAttacked gs 
                     && 
                     noEscape gs    
                     = Checkmate
                   | kingAttacked gs 
                     = Check
                   | otherwise     
                     = InPlay

sides  :: GameState -> Sides
sides gs = let p = Map.partition
                           (\x -> 
                              color x
                              == 
                              toPlay gs)
                           (board gs)
           in Sides{ 
               toPlaySide=(fst p),
               opponents=(snd p)
           }

kingPos :: GameState -> Position
kingPos gs = fst $ 
              head $
               Prelude.filter 
                (\x -> x==x) $
                Map.toList (board gs)

kingAttacked :: GameState -> Bool
kingAttacked gs = let kp = kingPos gs
                  in posAttacked kp gs

posAttacked :: Position -> GameState -> Bool
posAttacked pos gs = 
   or [
       attacks (snd posPiece) 
         AttackArgs {
          fromPos=(fst posPiece),
          toPos=pos,
          attackers=opponents (sides gs),
          defenders=toPlaySide (sides gs)
         } 
        |
        posPiece <- 
        Map.toList $ opponents (sides gs)
      ]

attacks :: Piece -> AttackArgs -> Bool
attacks piece attArgs
      | pieceType piece == Rook 
        = orthogAttack attArgs Unlimited
      | pieceType piece == Bishop 
        = diagAttack attArgs Unlimited
      | pieceType piece == Queen 
        = or [
          orthogAttack attArgs Unlimited,
          diagAttack attArgs Unlimited
          ]
      | pieceType piece == King
        = or [
          orthogAttack attArgs OneSquare,
          diagAttack attArgs OneSquare
          ]
      | otherwise               
        = False

orthogAttack :: AttackArgs -> MvLimit -> Bool
orthogAttack attArgs Unlimited = 
  or [
    attacksDir dir attArgs |
    dir <- orthDirs
    ]

diagAttack :: AttackArgs -> MvLimit -> Bool
diagAttack attArgs Unlimited = True

attacksDir :: Dir -> AttackArgs -> Bool
attacksDir dir attArgs =
    case nextSquare dir (fromPos attArgs) of
          Just nextPos 
            | (toPos attArgs) == nextPos   -> True
            | otherwise -> attacksDir dir attArgs{fromPos=nextPos}
          Nothing      -> False


orthDirs :: [Dir]
orthDirs = [North, South, East, West]
diagDirs :: [Dir]
diagDirs = [NE, NW, SE, SW]

nextSquare :: Dir -> Position -> Maybe Position
nextSquare North (r1, c1) 
    | r1 == maxBound = Nothing
    | otherwise      = Just (succ r1, c1)
nextSquare South (r1, c1) 
    | r1 == minBound = Nothing 
    | otherwise      = Just (pred r1, c1)
nextSquare East (r1, c1) 
    | c1 == maxBound = Nothing
    | otherwise      = Just (r1, succ c1)
nextSquare West (r1, c1) 
    | c1 == minBound = Nothing
    | otherwise      = Just (r1, pred c1)

occupied :: Position -> AttackArgs -> Occupancy
occupied pos attArgs
    | Map.lookup pos (attackers attArgs) /= Nothing = Attacker
    | Map.lookup pos (defenders attArgs) /= Nothing = Defender
    | otherwise = Empty

noEscape :: GameState -> Bool
noEscape gs = 
  and 
    [
      do 
        True
      | _ <- [1..5]
    ]

possiblePcMoves :: (Position, Piece) -> Board -> [Board]
possiblePcMoves (pos, piece) board =
      case (pieceType piece) of
        Rook    -> orthogMoves pos board Unlimited
        Bishop  -> []
        Queen   -> []
        King    -> [] 

orthogMoves :: Position -> Board -> MvLimit ->  [Board]
orthogMoves _ _ _ = []


move :: Position -> Position -> Board -> Board
move pos1 pos2 board  = 
  case (Map.lookup pos1 board) of
    Just v   -> delete pos1 $ insert pos2 v board
    Nothing  -> delete pos1 board
  
  
possibleMoves :: GameState  -> [Board] 
possibleMoves gs =
   [
     board
     |
      (pos, piece) <-
         Map.toList $ toPlaySide (sides gs),
         board <- possiblePcMoves (pos,  piece) (board gs)
   ]


main = do
         let gameState = game2
         putStrLn $ show (kingPos gameState)
         putStrLn $ show (sides gameState)
         report $ determineStatus gameState


report status = putStrLn $ show status

                   
                             
--fromList :: Ord k => [<(k, a)] -> Map k aSource

board1 :: Board
board1 = Map.fromList [
    ((Two,B), Piece King Black),
    ((Three,C), Piece Rook White)
  ]

game1 :: GameState
game1 = GameState {
  board=board1,
  toPlay=Black
}

board2 :: Board
board2 = Map.fromList [
    ((Two,B), Piece King Black),
    ((Two,F), Piece Rook White)
  ]
  
game2 :: GameState
game2 = GameState {
  board=board2,
  toPlay=Black
}


--status InPlay             
