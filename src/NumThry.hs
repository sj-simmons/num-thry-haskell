module NumThry
( sieve
, primes
, xgcd
, prhoFactor
) where 


sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primes :: [Int]
primes = sieve [2..]

iter :: (Integral a) => a -> a -> (a, a) -> (a, a) -> (a, a, a)
iter a b (s0, s1) (t0, t1) 
    | a == 0 = (b, s1 , t1) 
    | b == 0 = (a, s0 , t0)
    | otherwise = iter (a `mod` b)  (b `mod` a) (s0 - quot a b * s1, s1 - quot b a * s0)  (t0 - quot a b * t1, t1 - quot b a * t0)

-- return triple (gcd(a,b), m, n) where gcd(a,b) = m * a + n * b 
xgcd :: (Integral a) => a -> a -> (a, a, a) 
xgcd a b = iter a b (1, 0) (0, 1)


--gcd :: (Integral a) => a -> a -> a 
--gcd x y 
--    | y == 0 = x 
--    | otherwise = gcd y (mod x y)

f :: (Integral a) => a -> a
f x = ( x * x + 1 ) 

findCycle :: (Integral a) => (a, a) -> a -> a
findCycle (x, y) n
--    | d == n = 0
    | d > 1 = d
    | otherwise = findCycle (iter (x, y)) n
    where 
        d = gcd (abs x-y) n 
        iter (a, b) = ( f a `mod` n, f (f b) `mod` n)
   
testFindCycle :: Integer
testFindCycle = findCycle (f 2, f (f 2)) 20
--  trace   (5, 6)  d = gcd(1, 20) = 1
--          (6, 17) d = gcd(11, 20) = 1
--          (17, 9) d = 4

prhoFactor :: (Integral a) => a -> a
prhoFactor n = findCycle (f 2, f (f 2)) n
