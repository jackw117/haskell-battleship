--- Given Library Code
--- ==================

module Lib where

import Control.Monad.State
import Control.Monad.Fail
import Data.Char
import Debug.Trace
import System.Random

-- custom types
-- game represents the 4 boards of the current game
-- output represents what is printed out for the user after a command
-- ships represents the ships that still need to be placed
-- StdGen represents the current seed for the pseudo random number generator used by the computer
type GameState = (Game, Output, Ships, StdGen)
type Game   = [Board]
type Ships = [Int]
type Board  = [Char]
type Output = String

type RandState a = State StdGen a
type GState a = State GameState a

-- data type for user inputs
data Command = Print
             | Fire Char Int
             | Quit
             | Place Char Int Char Int
             | New
             | Error String

instance Show Command where
  show (Error e) = e
  show (Place c1 i1 c2 i2) = "Place " ++ show c1 ++ " " ++ show i1 ++ " " ++ show c2 ++ " " ++ show i2

-- global variables
size = 10
unknown = '_'
here = 'H'
hit = 'X'
miss = '.'


-- functions to create the initial GameState


makeBoard :: Board
makeBoard = unknown : makeBoard

initialBoard :: Board
initialBoard = take (size * size) makeBoard

initialOutput :: Output
initialOutput = ""

initialShips :: Ships
initialShips = [5, 4, 3, 3, 2]

initialGame :: RandState Game
initialGame = do
  r <- get
  let b = evalState (randomBoard initialShips initialBoard) r
  return $ initialBoard:initialBoard:b:initialBoard:[]

initialState :: RandState GameState
initialState = do
  r <- get
  let game = evalState initialGame r
  return (game, initialOutput, initialShips, r)

-- generates a random board for the computer opponent
randomBoard :: Ships -> Board -> RandState Board
randomBoard [] b =
  return b
randomBoard (x:xs) b = do
  -- randomR throws an error if the Ints aren't declared as Int
  g <- get
  let (vh, _) = randomR (0 :: Int, 1 :: Int) g
  case vh of
     -- get random X coordinate and keep Y coordinate the same
     0 -> do
       list <- randomHorizontal x
       b2 <- testRandom list size b x
       result <- randomBoard xs b2
       return result
     -- get random Y coordinate and keep X coordinate the same
     1 -> do
       list <- randomVertical x
       b2 <- testRandom list 1 b x
       result <- randomBoard xs b2
       return result

-- tests whether the ship (represented as a list of ints) can be displayed on the current board
testRandom :: [Int] -> Int -> Board -> Int -> RandState Board
testRandom list d b s = do
  g <- get
  -- disp represents the displacement that is added to each element in the list
  -- it is basically a random starting point for the partially calculated ship
  let (disp, g2) = randomR (0 :: Int, (size - 1) :: Int) g
      sumList = map (+ (disp * d)) list
  put g2
  -- place ship on board if there is no overlap
  if notTaken sumList b then do
     return (placeShip sumList b)
  -- start over with a new ship otherwise
  else do
     result <- randomBoard [s] b
     return result

-- generate random horizontal placement for a ship of the given size
randomHorizontal :: Int -> RandState [Int]
randomHorizontal i = do
  g <- get
  let (start, g2) = randomR (0 :: Int, (size - i) :: Int) g
      end = start + i - 1
  put g2
  return [start..end]

-- generate random vertical placement for a ship of the given size
randomVertical :: Int -> RandState [Int]
randomVertical i = do
  g <- get
  let (start, g2) = randomR (0 :: Int, (size - i) :: Int) g
      adjStart = size * start
      end = adjStart + ((i - 1) * size)
  -- filter out the numbers indices that would not be vertical
  -- (will keep something like [0,10,20] for ship size 3)
  put g2
  return (filter (\x -> x `mod` size == 0) [adjStart..end])


-- Printing functions


-- displays the numbers along the top of a printed game
topBorder :: Int -> String
topBorder i =
  if i == (size + 1) then
    "\n\n"
  else
    show i ++ "   " ++ topBorder (i + 1)

-- generates a String representation of the given board with numbers along the top
-- and letters on the left side to determine coordinates
printBoard :: Int -> Board -> String -> Char -> Output
printBoard 0 [] s c = s ++ "\n\n"
printBoard 0 xs s c =
  let ch = toEnum ((fromEnum c) + 1)::Char
   in printBoard size xs (s ++ "\n\n" ++ [ch] ++ "   ") ch
printBoard n (x:xs) s c = printBoard (n - 1) xs (s ++ [x] ++ "   ") c

-- prints a board with the given index
-- (1 == player's ships, 2 == player's guesses, 3 == comp ships, 4 == comp guesses)
printGame :: Int -> GState Output
printGame i = do
  (bs, out, ships, r) <- get
  case i <= 4 && i >= 1 of
    True -> return $ out ++ (printBoard size (bs!!(i-1)) ("    " ++ (topBorder 1) ++ "A   ") 'A')
    False -> return $ out ++ "Error: " ++ show i ++ " is not a valid board number\n"


-- Firing functions

-- fires at a random coordinate from the list of coordinates that haven't been guessed yet
randFire :: GState GameState
randFire = do
  (bs, out, sh, r) <- get
  let b4 = bs!!3
      list = getUnkList b4 0
      (i, r2) = randomR (0, (length list) - 1) r
  g2 <- fireInt (list!!i)
  put g2
  return g2

-- returns a list of integers corresponding with the coordinates the computer hasn't guessed yet
getUnkList :: Board -> Int -> [Int]
getUnkList [] _ = []
getUnkList (b:bs) n =
  if (b == unknown) then
    n : getUnkList bs (n+1)
  else
    getUnkList bs (n+1)

-- returns the first unknown coordinate surrounding a hit
-- used by the computer to make a better guess on where to fire next
getSurrounding :: Board -> Board -> Int -> Int
getSurrounding _ [] _ = -1
getSurrounding bstatic (b:bs) i =
  if b == hit then do
    let top = i - size
        left = i - 1
        right = i + 1
        down = i + size
    if top >= 0 && bstatic!!top == unknown then
      top
    else if left >= 0 && bstatic!!left == unknown then
      left
    else if right >= 0 && bstatic!!right == unknown then
      right
    else if down < (size * size) && bstatic!!down == unknown then
      down
    else
      getSurrounding bstatic bs (i+1)
  else
    getSurrounding bstatic bs (i+1)

-- fires with a coordinate for the computer's turn
compFire :: GState GameState
compFire = do
  (bs, out, sh, r) <- get
  let b4 = bs!!3
      gh = getSurrounding b4 b4 0
  if gh == -1 then do
    result <- randFire
    return result
  else do
    result <- fireInt gh
    return result

-- fires with an index instead of coordinates
-- used with the computer firing functions only
fireInt :: Int -> GState GameState
fireInt i = do
  (b, out, sh, r) <- get
  let b2 = b!!0
  let b1 = b!!3
  if b2!!i == here then
    return ((replaceIndex i hit b2):(b!!1):(b!!2):(replaceIndex i hit b1):[], out, sh, r)
  else
    return (b2:(b!!1):(b!!2):(replaceIndex i miss b1):[], out, sh, r)

-- fire with the given coordinates
fire :: Char -> Int -> Board -> Board -> (Board, Board, String)
fire c i b1 b2 =
  let index = (ord (toUpper c) - 65) * size + i - 1
   in if b2!!index == here || b2!!index == hit then
        (replaceIndex index hit b1, replaceIndex index hit b2, "Kaboom!\n")
      else
        (replaceIndex index miss b1, b2, "Sploosh...\n")

-- determines if the given int is in the bounds of the game (1 <= i <= size)
inBounds :: Int -> Bool
inBounds i = (abs i) <= size

-- places a ship on the given board using the given indices
placeShip :: [Int] -> Board -> Board
placeShip [] b = b
placeShip (x:xs) b = placeShip xs (replaceIndex x here b)

-- determines if the given ship can be placed on the board
-- False if any section of the ship will overlap with another ship
notTaken :: [Int] -> Board -> Bool
notTaken [] _ = True
notTaken (x:xs) b = b!!x /= here && notTaken xs b

-- removes the given ship from the Ships array after it has been placed
removeShip :: Int -> Ships -> Ships
removeShip _ [] = []
removeShip i (x:xs) =
  case i == x of
    -- only remove the first ship of a given size
    True -> removeShip (-1) xs
    False -> x : removeShip i xs

-- returns a list of the indices for a ship based on the given start and end coordinates
placementList :: Char -> Int -> Char -> Int -> [Int]
placementList c1 i1 c2 i2 =
  let start = ((ord (toUpper c1) - 65) * size) + i1 - 1
      end = ((ord (toUpper c2) - 65) * size) + i2 - 1
      mn = min start end
      mx = max start end
   in [mn..mx]

-- replaces given index of a board with the given char (used with placement and firing)
replaceIndex :: Int -> Char -> Board -> Board
replaceIndex _ _ [] = []
replaceIndex i n (x:xs) =
 case i of
   0 -> n:xs
   _ -> x:replaceIndex (i-1) n xs

-- places a ship in the GameState cooresponding with the given start and end coordinates
place :: Char -> Int -> Char -> Int -> GState GameState
place c1 i1 c2 i2 = do
  (b, out, ships, r) <- get
  let b1 = head b
      b2 = tail b
  -- horizontal
  if c1 == c2 then
    let list = placementList c1 i1 c2 i2
     -- check that coordinate size is a valid ship size
     in if elem (length list) ships && notTaken list b1 then
          let newShips = removeShip (length list) ships
           in return ((placeShip list b1):b2, "Placed\n", newShips, r)
        else
          return (b, "Error: not valid coordinates\n", ships, r)
  -- vertical
  else if i1 == i2 then
    let list = filter (\x -> x `mod` size == (i1 - 1)) (placementList c1 i1 c2 i2)
     -- check that coordinate size is a valid ship size
     in if elem (length list) ships && notTaken list b1 then
          let newShips = removeShip (length list) ships
           in return ((placeShip list b1):b2, "Placed\n", newShips, r)
        else
          return (b, "Error: not valid coordinates\n", ships, r)
  -- can't be diagonal
  else
    return (b, "Error: can't be diagonal\n", ships, r)


-- Functions to determine the winner


-- determines the number of times a given element is found in a list
-- used to determine if there is a winner
-- copied from:
-- https://codereview.stackexchange.com/questions/139587/count-occurrences-of-an-element-in-a-list
numTimesFound :: Ord a => a -> [a] -> Int
numTimesFound _ [] = 0
numTimesFound x list = sum $ map (\a -> 1) $ filter (== x) list

-- determines if there is a winner of the game
-- (# of hits on a guess board == sum of the initial ships) means that the owner of that board is the winner
win :: Board -> Bool
win b = numTimesFound hit b == sum initialShips


-- Evaluating the Command data type

eval :: Command -> GState GameState
eval (Print) = do
  (b, _, sh, r) <- get
  out1 <- printGame 1
  out2 <- printGame 2
  return (b, "Your ships:\n" ++ out1 ++ "Your hits/misses on enemy ships:\n" ++ out2, sh, r)
eval (Fire c i) = do
  (bs, out, sh, r) <- get
  let (newB1, newB2, check) = fire c i (bs!!1) (bs!!2)
      newG = ((bs!!0):newB1:newB2:(bs!!3):[], out, sh, r)
  put newG
  g2 <- compFire
  put g2
  (g3, o3, s3, r3) <- (eval Print)
  return (g3, o3 ++ check, s3, r3)
eval (Place c1 i1 c2 i2) = do
  g <- get
  g2@(b, _, sh, r) <- place c1 i1 c2 i2
  put g2
  out2 <- printGame 1
  return (b, out2, sh, r)
eval (New) = do
  (_, _, _, r) <- get
  let g@(b, _, sh, r2) = evalState initialState r
  put g
  out <- printGame 1
  return (b, "New game\n" ++ out, sh, r2)
