module Main where
import Lib
import Parser

import Prelude
import System.IO
import Control.Monad.State
import Text.ParserCombinators.Parsec hiding (State)
import System.Environment
import Data.Char
import Data.Typeable
import Control.Monad.Except
import System.Random

--- ### REPL

repl :: GameState -> IO ()
repl s1@(bs, output, ships, r) = do
  r2 <- newStdGen
  let s = (bs, output, ships, r2)

  -- testing for comp placement and firing

  -- let ot = evalState (printGame 4) s
  -- putStrLn ot
  -- let num1 = numTimesFound miss (bs!!3)
  --     num2 = numTimesFound hit (bs!!3)
  -- putStrLn (show $ num1 + num2)
  -- let (bt, ot, st, rt) = evalState (printGame 4) s
  -- putStr ot

  -- check if user has won
  if win (bs!!2) then do
    putStrLn "You win!"
    let ot = evalState (printGame 3) s
    let ot2 = evalState (printGame 4) s
    putStrLn "Computer ships:"
    putStrLn ot
    putStrLn "Computer guesses:"
    putStrLn ot2
    return()

  -- check if computer has won
  else if win (bs!!0) then do
    putStrLn "You lose..."
    let ot = evalState (printGame 3) s
    let ot2 = evalState (printGame 4) s
    putStrLn "Computer ships:"
    putStrLn ot
    putStrLn "Computer guesses:"
    putStrLn ot2
    return()

  -- check if user still needs to place down ships
  -- user cannot execute other commands during this phase (aside from quit and new game)
  else if length ships /= 0 then do
    putStrLn "Use the place command to place your ships on the board"
    putStrLn $ "Current ships: " ++ show ships
    putStr "> "
    input <- getLine
    let val = readExpr 1 input
    readCases val s

  -- user has placed down all their ships and can start firing
  -- user cannot place ships during this phase
  else do
    putStr "> "
    input <- getLine
    let val = readExpr 2 input
    readCases val s

-- the 3 cases for parsing a command
readCases :: Command -> GameState -> IO ()
readCases val s =
  case val of
    -- bad format or invalid command
    (Error e) -> do
      putStrLn (e)
      repl s
    -- quit the game
    (Quit) -> do
      putStrLn "Bye!"
      return ()
    -- evaluate the given command and recall the repl with the new state
    _ -> do
      let (g, out, sh, r) = evalState (eval val) s
      putStrLn out
      repl (g, "", sh, r)

main :: IO ()
main = do putStrLn "Welcome to Battleship!"
          r <- newStdGen
          readCases New (evalState initialState r)
