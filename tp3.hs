module Class03 where

import Data.Complex
import System.Random
import Data.List

-- function from Problem Set 1
-- Matrix multiplication
gate :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
gate m n = [[sum $ zipWith (*) mr nc | nc <- (transpose n)] | mr <- m]

-- Tensor product
tensor :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
tensor m n = [[ a*b | a <- mr, b <- nr] | mr <- m, nr <-n] 

-- functions from Problem Set 2
s0 :: [[Complex Float]]
s0 = [[1],[0]]

s1 :: [[Complex Float]]
s1 = [[0],[1]]

had ::[[Complex Float]]
had = [[h,h],[h,-h]]
   where 
      h = 1/sqrt(2)

idgate :: [[Complex Float]]
idgate = [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]

cnot :: [[Complex Float]]
cnot = [[1,0,0,0],[0,1,0,0],[0,0,0,1],[0,0,1,0]]

s00 :: [[Complex Float]]
s00 = tensor s0 s0

bell ::[[Complex Float]]
bell = gate(gate cnot(tensor had idgate)) s00
----------------------------------------------------------
--1a
amplitude_acc :: [[Complex Float]] -> [Float]
amplitude_acc m = scanl1 (+) [realPart ((head x) * (conjugate(head x))) | x<-m] 

--1b
meas_acc :: [Float] -> Float -> [Float]
meas_acc [] _ = []
meas_acc (h:t) r
	| r >= h = 0: (meas_acc t r)
	| r < h  = 1: (take (length t)(repeat 0))

--1c

state_to_char :: [Float] -> [Char]
state_to_char [] = []
state_to_char (li:[]) = []
state_to_char l
    | 1.0 `elem` lf = '0' : (state_to_char lf)
    | 1.0 `elem` ls = '1' : (state_to_char ls)
    where 
    	lf = fst $ splitAt (quot (length l) 2) l
        ls = snd $ splitAt (quot (length l) 2) l

--1d
meas ::[[Complex Float]] -> IO [Char]
meas v = do
	n <- randomIO :: IO Float
	return $ state_to_char $ meas_acc (amplitude_acc v) n

--2a
shots :: [[Complex Float]] -> Int -> IO [[Char]]
shots v n = undefined

--2b
freqs ::[[Complex Float]] -> Int -> IO [([Char], Int)]
freqs v n = undefined
