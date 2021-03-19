module Class02 where

import Data.Complex

-- Complex Float
-- 2 :+ (-1) represents 2-i  

-- function from Problem Set 1
-- Transpose 
transp :: [[Complex Float]] -> [[Complex Float]]
transp [] = []
transp ([] : _ ) = []
transp row = ((map head row) : transp (map tail row))

-- Matrix multiplication
gate :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
gate m n = [[sum $ zipWith (*) mr nc | nc <- (transp n)] | mr <- m]

-- Tensor product
tensor :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
tensor m n = [[ a*b | a <- mr, b <- nr] | mr <- m, nr <-n] 

--------------------------------------------------------------------
--1
s0 :: [[Complex Float]]
s0 = [[1],[0]]

s1 :: [[Complex Float]]
s1 = [[0],[1]]

hadamard :: [[Complex Float]]
hadamard = [[1/sqrt(2), 1/sqrt(2)], [1/sqrt(2), -1/sqrt(2)]]

plus :: [[Complex Float]]
plus = gate hadamard s0

minus :: [[Complex Float]]
minus = gate hadamard s1
--2 a)
--00
s00 ::[[Complex Float]]
s00 = tensor s0 s0
--11
s11 :: [[Complex Float]]
s11 = tensor s1 s1

--2 b)
s010 :: [[Complex Float]]
s010 = tensor (tensor s0 s1) s0

--3 a)
-- Hint use function conjugate
-- >x = 2 :+ (-1)
-- >conjugate x
-- 2:+ 1
vconjugate :: [Complex Float] -> [Complex Float]
vconjugate [] = []
vconjugate (x:xs) = (conjugate x) : vconjugate xs

cnorm :: [[Complex Float]] -> Complex Float
cnorm [] = 0
cnorm ([]:xs) = cnorm xs
cnorm m = sqrt(sum(zipWith (*) (head m) (vconjugate (head m))) + cnorm (tail m))

norm :: [[Complex Float]] -> Complex Float
norm v = sqrt (sum [ (head a)*(conjugate(head a))|a<-v])

--3 b)
normalise :: [[Complex Float]] -> [[Complex Float]]
normalise m = [[x/norm(m) | x <- y] | y <- m]

--4 a)
--hint: recall cis 
--cis x = cos x + i sin x = e^{ix}
u3 :: (Float, Float, Float) -> [[Complex Float]]
u3 (t, p, l) = [[cos(t/2):+ 0.0, -cis(l)*(sin(t/2):+0.0)], [cis p*(sin(t/2):+0.0),cis(p+l)*(cos(t/2):+0.0)]]

--4 b)

au3 = u3(0,pi,0)
--

bu3 = u3(pi/2,0,pi)
--

--5 a)
cn :: [[Complex Float]]
cn = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]]

systa :: [[Complex Float]]
systa = gate cn (tensor (gate hadamard s0) s0)

--5 b)
systb :: [[Complex Float]]
systb = (hadamard `tensor` hadamard) `gate` (cn `gate` ((hadamard `tensor` hadamard) `gate` s00))  

--6
lin a b = (replicate a (0:+0)) ++ [1:+0] ++ (replicate b (0:+0))

auxcnot x ls
    | x <= ls = lin (x-1) (ls*2-x)
    | mod x 2 == 0 = lin (x-2) (ls*2-(x+1))
    | otherwise = lin (x) (ls*2-(x-1))


cnot_ :: Int -> [[Complex Float]]
cnot_ n = [(auxcnot x l) | x <- [1..l]] ++ [auxcnot x l | x <- [(l+1)..(l*2)] ]
    where 
    	l = quot(2^(n)) 2
