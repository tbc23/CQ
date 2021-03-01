--1a
perimeter_circle :: Floating a => a -> a
perimeter_circle r = 2 * pi * r

--1b
area_circle :: Floating a => a -> a
area_circle r = pi * r ** 2 

--2
factorial_ :: Int -> Int
factorial_ 0 = 1
factorial_ x = x*factorial_(x-1)

--3a
all_numbers_upto_n :: Int -> [Int]
all_numbers_upto_n n = [2..n]

--3b
elim_numbers :: Int -> [Int] -> [Int]
elim_numbers x l = [i | i <- l, mod x i /= 0]

--3c
auxprime :: [Int] -> [Int]
auxprime [] = [] 
auxprime (h:t) = h: auxprime(elim_numbers h t)

prime_list :: Int -> [Int]
prime_list n = auxprime(all_numbers_upto_n n)

--3d
isprime :: Int -> Bool
isprime x = elem x (prime_list x)

--3e
factorize_prime :: Int -> [Int]
factorize_prime n = [x | x <- (prime_list n) , (mod n x == 0)]

--4a
transp :: Num a => [[a]] -> [[a]]
transp [] = []
transp ([]:_) = []
transp x = (map head x : transp (map tail x))

--4b
mult :: Num a => [[a]] -> [[a]] -> [[a]]
mult m n = [[sum $ zipWith (*) mr nc | nc <- (transp n)] | mr <- m]

--4c
tensor_p :: Num a => [[a]] -> [[a]] -> [[a]]
tensor_p m n = [[ a*b | a <- mr, b <- nr ] | mr <- m , nr <- n ]

--4d
projection :: Num a => [[a]] -> [[a]] -> [[a]]
projection m n = tensor_p m $ transp n

--5
gate :: Num a => [[a]] -> [[a]] -> [[a]]
gate cnot v = mult cnot v
