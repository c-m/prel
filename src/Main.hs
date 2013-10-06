
module Main where

import Data.Char
import Data.List
import Control.Monad

-- |This is the main function (the center of all that it was, it is and will be).
-- Given n and a set of functions, print the final result after aplying the functions
main :: IO ()
main = do
	putStrLn "Enter the input source: "
	getLine >>= readFile >>= compute
	
	putStrLn "Quit the program? yes/no"
	ans <- getLine
	when (ans /= "yes") $ do
		putStrLn "Not quitting..."
		main

compute :: String -> IO ()
compute [] = putStrLn "The file is empty!"
compute xs = putStrLn $ "The number is:\n" 
	++ (show $ getNumber xs) ++ "\nThe functions are:\n" 
	++ (show $ getFunctions xs) ++ "\nThe results are:\n" 
	++ (show $ evaluate xs)
	
evaluate :: String -> [Int]
evaluate xs = apply (separate xs) (getNumber xs) 
	
separate :: String -> [String]
separate xs = 
				let 
					lastEl = last (words $ getFunctions xs)
				in map init ( init (words $ getFunctions xs))  ++ [lastEl]

getNumber :: String -> Int
getNumber xs = read $ lines xs !! 0 :: Int
	
getFunctions :: String -> String
getFunctions xs
					| length (lines xs) == 1 = error "No functions in the file\n"
					| otherwise = init $ tail $ lines xs !! 1

apply :: [String] -> Int -> [Int]
apply [] _ = []
apply (x:xs) n = ( functions x n) : ( apply xs n) 

functions :: String -> Int -> Int
functions x n
	|	isPrefixOf "add_" x 				= n + (value x)
	|	isPrefixOf "subtract_"       x		= n - (value x)
	|	isPrefixOf "multiply_by_"  x		= n * (value x)
	|	isPrefixOf "divide_by_"     x	= n `div` (value x)
	|	otherwise                           	= error "something is spelled wrong in your file!"

value :: String -> Int		
value x
	|	isSuffixOf "zero" 	x = 0
	|	isSuffixOf "one" 	x = 1
	|	isSuffixOf "two" 		x = 2
	|	isSuffixOf "three" 	x = 3
	|	isSuffixOf "four" 	x = 4
	|	isSuffixOf "five" 	x = 5
	|	isSuffixOf "six" 		x = 6
	|	isSuffixOf "seven" 	x = 7
	|	isSuffixOf "eight" 	x = 8
	|	isSuffixOf "nine" 	x = 9
	|	otherwise 				   = error "this integer digit does not exist!"