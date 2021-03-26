import Data.Map as Map

data Row       =   One  | Two  | Three 
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

determineStatus :: GameState -> Status
determineStatus gs | kingAttacked gs 
                     && 
                     noEscape GameState   = Checkmate
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
                      opps = opponents (sides gs)
                  in posAttacked kp opps gs

posAttacked :: Position -> Map Position Piece -> GameState -> Bool
posAttacked pos opps gs = 
   or [
       attacks (snd posPiece) 
         AttackArgs {
          fromPos=(fst posPiece),
          toPos=pos,
          attackers=opps,
          defenders=toPlaySide (sides gs)
         } 
        |
        posPiece <- 
        Map.toList opps
      ]

attacks :: Piece -> AttackArgs-> Bool
attacks piece attArgs
      | pieceType piece == Rook 
        = orthogAttack fPos tPos  atts defs Unlimited
      | pieceType piece == Bishop 
        = diagAttack fPos tPos  atts defs Unlimited
      | pieceType piece == Queen 
        = or [
          orthogAttack fPos tPos  atts defs Unlimited,
          diagAttack fPos tPos  atts defs Unlimited
          ]
      | pieceType piece == King
        = or [
          orthogAttack fPos tPos  atts defs OneSquare,
          diagAttack fPos tPos  atts defs OneSquare
          ]
      | otherwise               
        = False
      where
          fPos = fromPos attArgs
          tPos = toPos attArgs
          atts = attackers attArgs
          defs = defenders attArgs
          

orthogAttack :: Position -> Position -> Map Position Piece -> Map Position Piece -> MvLimit -> Bool
orthogAttack fromPos toPos attackers defenders Unlimited = True

diagAttack :: Position -> Position -> Map Position Piece -> Map Position Piece -> MvLimit -> Bool
diagAttack fromPos toPos attackers defenders Unlimited = True



noEscape     _ = False

getColor :: Piece -> Color
getColor (Piece _ c) = c

--ghc 8.6.3

main = do
         let gameState = game1
         putStrLn $ show (kingPos gameState)
         putStrLn $ show (sides gameState)
         report $ determineStatus gameState


report status = putStrLn $ show status

                   
                             
--fromList :: Ord k => [<(k, a)] -> Map k aSource

board1 :: Board
board1 = Map.fromList [
    ((One,B), Piece King Black),
    ((Three,C), Piece Rook White)
  ]

game1 :: GameState
game1 = GameState {
  board=board1,
  toPlay=Black
}

--status InPlay             
