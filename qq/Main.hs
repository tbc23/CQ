module Main where

import Quipper
import Quipper.Internal.Printing
import pyzx as zx

import System.Random

a = randomIO :: IO Float
 
plus_minus :: Bool -> Circ Qubit
plus_minus b = do
   q <- qinit b
   r <- hadamard q
   return r

print_plus_minus :: IO()
print_plus_minus = print_simple Preview (plus_minus False)

mycnot :: Bool -> Bool -> Circ(Qbuit,Qubit)
mycnot a b = do
	qa <- qinit a
	qb <- qinit b
	qb <- qnot qb `controlled` qa
	return (qa,qb)

print_mycnot :: IO()
print_mycnot = print_simple ASCII (mycnot True True)

my_qc = zx.Circuit.load("decompose")

main :: IO ()
main = putStrLn "Hello, Haskell!"
