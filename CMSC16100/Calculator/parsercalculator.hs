-- Parser Calculator

module Main where

import Data.Char
import Control.Monad
import Data.List
import Data.List.Split
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import System.IO

--1.1 Defining an ArithExpr Data Type
data ArithExpr a = Number a
                 | Plus (ArithExpr a) (ArithExpr a)
                 | Mult (ArithExpr a) (ArithExpr a)
    deriving (Show, Eq)

testExpr1 = (Plus (Plus (Number 1) (Number 2)) (Number 3))
testExpr2 = (Plus (Mult (Number 4) (Number 2)) (Number 3))
testExpr3 = (Plus (Number 1) (Plus (Mult (Number 2) (Number 3)) (Mult (Mult (Number (-5)) (Number 3)) (Plus (Number 4) (Number (-2)))))

--1.2 Evaluating ArithExpr
eval :: (Num a, Monad m) => ArithExpr a -> m a
eval (Number x) = return x
eval (Plus x y) = liftM2 (+) (eval x) (eval y)
eval (Mult x y) = liftM2 (*) (eval x) (eval y)

testEvalExpr1 = (eval (Num 5)) == 5
testEvalExpr2 = (eval (Plus (Number 4) (Number 2))) == 6
testEvalExpr3 = (eval (Mult (Plus (Number 4) (Number 3)) (Mult (Number 4) (Number 2)))) == 56
testEvalExpr4 = (eval (Plus (Plus (Mult (Number 3) (Number 2)) (Mult (Number 4) (Number 6))) (Mult (Number 5) (Number 3))) == 45

--1.3 Parsing Strings

--1.3.1 Removing Whitespace
--(not . isSpace) handles "", "\n", and "\t".
whitespace :: String -> String
whitespace myString = filter (not . isSpace) myString

testWhitespace1 = whitespace "2 + 5 *\t4\n3"
testWhitespace2 = whitespace "\n 2 +    4 * 4\t "

--1.3.2 Tokenising
--Using Data.List.Split, default setting for (split (oneOf)) splits at every indicated 
--delimiter and retains the delimiter as a token.
tokenize xs = split (oneOf "+*") xs

testTokenize1 = tokenize "24"
testTokenize2 = tokenize "-100"
testTokenize3 = tokenize "30+20"
testTokenize4 = tokenize "20+-10"
testTokenize5 = tokenize "20+3-4*2"
testTokenize6 = tokenize "10*4*3+3+4+-4"

--1.3.3 Turning tokens into an ArithExpr
--Adapted from Text.ParserCombinators.Parsec.Expr documentation and examples
buildExpressionParser :: OperatorTable tok st a -> GenParser tok st a -> GenParser tok st a
buildExpr = buildExpressionParser table factor

table = [[ArithExpr "+" (Plus) AssocLeft]
        ,[ArithExpr "*" (Mult) AssocLeft]]
        where ArithExpr s f assoc = Infix (do string s; return f) assoc
        
factor = do char '(' ; x <- buildExpr ; char ')'
            return x
         <|> do ds <- many1 digit
            return $ Number (read ds)

testBuildExpr1 = buildExpr ["5","+","5"]
testBuildExpr2 = buildExpr ["5","*","5"]
testBuildExpr3 = buildExpr ["5","*","5","+","4","+","3"]
testBuildExpr4 = buildExpr ["5","+","-4"]

--1.3.4 Implementing simpleParseExpr
simpleParseExpr :: String -> ArithExpr
simpleParseExpr myString = buildExpr $ tokenize $ whitespace myString

testSimpleParseExpr1 = simpleParseExpr "1 + 5 * 3"
testSimpleParseExpr2 = simpleParseExpr " 45*3 +4"

--1.4 Interacting with the User
main :: IO ()
main = do
       x<-getString
       calculate string
       main

calculate :: String -> ArithExpr
calculate s = eval (simpleParseExpr s)
       
