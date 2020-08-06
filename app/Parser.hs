module Parser where
import Lib

import Text.ParserCombinators.Parsec hiding (State, spaces)
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import System.Environment
import Data.Char
import Control.Monad

-- I used these two guides to help write the parser
-- https://wiki.haskell.org/Parsing_a_simple_imperative_language
-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

-- defines the user inputs that are skipped over when parsing as a Command data type
-- these reserved names are used to determine which Command data type to pack the input into
languageDef =
 emptyDef {
            Token.reservedNames   = [ "print"
                                    , "fire"
                                    , "place"
                                    , "quit"
                                    , "new"
                                    ]
          }

lexer = Token.makeTokenParser languageDef
reserved = Token.reserved lexer

spaces :: Parser ()
spaces = skipMany1 $ oneOf " \n\t"

-- Command parsers

printC :: Parser Command
printC = do
  reserved "print"
  return Print

quitC :: Parser Command
quitC = do
  reserved "quit"
  return Quit

newC :: Parser Command
newC = do
  reserved "new"
  return New

fireC :: Parser Command
fireC = do
  reserved "fire"
  c <- letter
  spaces
  i <- many1 digit
  if inBounds (ord (toUpper c) - 64) && inBounds (read i) then
    return (Fire c (read i))
  else
    return (Error "Coordinates not in bounds")

placeC :: Parser Command
placeC = do
  reserved "place"
  c1 <- letter
  spaces
  i1 <- many1 digit
  spaces
  c2 <- letter
  spaces
  i2 <- many1 digit
  if inBounds (ord (toUpper c1) - 64) && inBounds (read i1) && inBounds (ord (toUpper c2) - 64) && inBounds (read i2) then
    return (Place c1 (read i1) c2 (read i2))
  else
    return (Error "Coordinates not in bounds")

-- used for new games when ships still need to be placed down
-- only the place, new, and quit commands are able to be called in this stage of the game
parseExpr1 :: Parser Command
parseExpr1 = placeC
         <|> newC
         <|> quitC

-- used for when all the ships are placed down
-- the place command can no longer be called, and instead fire and print are able to be called
parseExpr2 :: Parser Command
parseExpr2 = fireC
         <|> printC
         <|> newC
         <|> quitC

-- determines which stage of the game to parse for based on the int parameter
-- (1 for the placement stage, and 2 for the firing stage)
readExpr :: Int -> String -> Command
readExpr i input =
  case i of
    1 ->
      case parse parseExpr1 "command" input of
        Left err -> Error (show err)
        Right val -> val
    2 ->
      case parse parseExpr2 "command" input of
        Left err -> Error (show err)
        Right val -> val
