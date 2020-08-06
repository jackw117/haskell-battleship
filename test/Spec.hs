module Spec where

import Test.HUnit
import Lib
import Parser
import Main hiding (main)
import Text.ParserCombinators.Parsec (parse)
import Control.Monad.State
import System.Random

main :: IO Counts
main = do
  r <- newStdGen
  let g = evalState initialState r
  runTestTT $ allTests g

allTests g = TestList [placeTests g, fireTests g, randomTests g]

placeTests g = TestList [TestLabel "test1" (test1 g),
                         TestLabel "test2" (test2 g),
                         TestLabel "test3" (test3 g),
                         TestLabel "test4" (test4 g)]

fireTests g = TestList [TestLabel "test 5" (test5 g),
                        TestLabel "test 6" (test6 g),
                        TestLabel "test 7" (test7 g)]

randomTests g = TestList [TestLabel "test 8" (test8 g)]

test1 g = do
  let val = readExpr 1 "place a 11 a 13"
  TestCase (assertEqual "(Place a 11 a 13) placement out of bounds,"
              ("Coordinates not in bounds") (show val))

test2 g = do
  let val = readExpr 1 "place a 1 a 5"
      (b, out, sh, r) = evalState (eval val) g
      b1 = b!!0
  TestCase (assertEqual "(Place a 1 a 5),"
              (here:here:here:here:here:[]) (b1!!0:b1!!1:b1!!2:b1!!3:b1!!4:[]))

test3 g = do
  let val = readExpr 1 "place a 1 a 5"
      g2 = evalState (eval val) g
      val2 = readExpr 1 "place a 1 a 4"
      (b, out, sh, r) = evalState (eval val2) g2
  TestCase (assertEqual "(Place a 1 a 5) + (Place a 1 a 4) placement on top of existing ship,"
              ("Error: not valid coordinates") (fst (splitAt 28 out)))

test4 g = do
  let val = readExpr 1 "place a 1 b 3"
      (b, out, sh, r) = evalState (eval val) g
  TestCase (assertEqual "(Place a 1 b 3) diagonal placement,"
              ("Error: can't be diagonal") (fst (splitAt 24 out)))

test5 (b1:b2:b3:b4:[], out, sh, r) = do
  let val = readExpr 2 "fire a 1"
      -- replace b3 with an empty board to test
      (b, o2, s2, r2) = evalState (eval val) (b1:b2:initialBoard:b4:[], out, sh, r)
  TestCase (assertEqual "(Fire a 1),"
              ((b!!1)!!0) (miss))

test6 g = do
  let val = readExpr 2 "fire k 1"
  TestCase (assertEqual "(Fire k 1) out of bounds,"
              ("Coordinates not in bounds") (show val))

test7 g = do
  let val = readExpr 1 "fire a 1"
  TestCase (assertEqual "(Fire a 1) before finished placing,"
              ("\"command\" (line 1, column 1):\nunexpected \"f\"\nexpecting \"place\", \"new\" or \"quit\"") (show val))

test8 (_, _, sh, r) = do
  let b = evalState (randomBoard sh initialBoard) r
      f = numTimesFound here b
  TestCase (assertEqual "random board correct size,"
              (sum initialShips) (f))
