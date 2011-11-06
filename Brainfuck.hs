module Main where

import System.Environment (getArgs)
import Control.Monad (liftM, foldM)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P (parse)
import Data.List.Zipper (Zipper, fromList, right, left, cursor, replace)
import Data.Char (ord, chr)
import Data.Word (Word8)

main :: IO ()
main = do
    args <- getArgs
    let path = parseArgs args
    code <- readFile path
    let ast = parse code
    let memory = emptyMemory 1024
    interpret memory ast
    return ()

parseArgs :: [String] -> String
parseArgs (x:[]) = x
parseArgs xs = error "Provide one argument, the path to the program"

parse :: String -> AST
parse code = case P.parse program "Brainfuck" (clean code) of
                  Left err  -> error $ show err
                  Right ast -> optimize ast

clean :: String -> String
clean [] = []
clean (x:xs) = if x `elem` "+-<>[].,"
                then x:clean xs
                else clean xs

type AST = [Instruction]

data Instruction = Add Int            -- Add positive/negative value
                 | Move Int           -- Move pointer
                 | Loop [Instruction]
                 | Input              -- Read one character
                 | Output             -- Write one character
                 deriving (Show, Eq)

program :: Parser AST
program = many instruction

instruction :: Parser Instruction
instruction = loop <|> operator

loop :: Parser Instruction
loop = liftM Loop $ between (char '[') (char ']') program

operator :: Parser Instruction
operator = liftM (Add . length)           (several '+')
       <|> liftM (Add . negate . length)  (several '-')
       <|> liftM (Move . length)          (several '>')
       <|> liftM (Move . negate . length) (several '<')
       <|> (char ',' >> return Input)
       <|> (char '.' >> return Output)
       where several c = many1 $ char c

optimize :: AST -> AST
optimize [] = []
optimize (Loop []:xs) = optimize xs
optimize (Loop xs:ys) = let optimizedContent = optimize xs
                            in if optimizedContent == []
                                  then optimize ys
                                  else Loop optimizedContent:optimize ys
optimize (Add m:Add n:xs) = if abs m == abs n && m /= n
                                   then Add (abs m):optimize xs
                                   else Add m:Add n:optimize xs
optimize (Move m:Move n:xs) = if abs m == abs n && m /= n
                                 then Move (abs m):optimize xs
                                 else Move m:Move n:optimize xs
optimize (x:xs) = x:optimize xs

type Memory = Zipper Word8

emptyMemory :: Int -> Memory
emptyMemory size = fromList $ replicate size 0

interpret :: Memory -> AST -> IO Memory
interpret = foldM step

step :: Memory -> Instruction -> IO Memory
step mem op = case op of
   Add n -> return $ replace (fromIntegral n + cursor mem) mem
   Move n -> return $ shift n mem
   Loop instructions -> performLoop mem instructions
   Output -> putChar (chr . fromEnum . cursor $ mem) >> return mem
   Input -> fmap (flip replace mem . fromIntegral . ord) getChar
   where performLoop mem ins = if cursor mem == 0
                                  then return mem
                                  else interpret mem ins >>= flip performLoop ins

shift :: Int -> Zipper a -> Zipper a
shift 0 zipper = zipper
shift 1 zipper = right zipper
shift (-1) zipper = left zipper
shift n zipper = if n > 0 then shift (n - 1) (shift 1 zipper)
                          else shift (n + 1) (shift (-1) zipper)
