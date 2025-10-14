-----------
-- Intro --
-----------

import Data.List
myInt = 31415926535897932384626433832795028841971693993751058209749445923

double :: Integer -> Integer
    double x = x+x

---------------------------
-- 4.1 Maximul a doua numere --
---------------------------

maxim :: Integer -> Integer -> Integer
maxim x y = 
    if (x > y)
        then x
        else y

---------------------------
-- 4.2 Maximul a trei numere --
---------------------------

max3 x y z = 
    let
        u = maxim x y
    in
        (maxim  u z)

----------------------------
-- 4.3 Maximul a patru numere --
----------------------------

-- Varianta 1: Cu recursivitate
maxim4 x y z t = 
    let
        u = max3 x y z
        in (maxim u t)

-- Varianta 2: Cu if-else
maxim4CuIf x y z t = 
    if (x >= y && x >= z && x >= t)
        then x
    else if (y >= x && y >= z && y >= t) 
        then y
    else if (z >= x && z >= y && z >= t) 
        then z
    else
        t

----------------------------------------
-- 6.1 Functie cu doi parametrii      --
-- car calculeaza suma patratelor lor --
----------------------------------------

sumaPatrateDoiParametrii x y =
    x * x + y * y

---------------------------------------
-- 6.2 Functie cu un parametru ce    --
-- intoarce stringul par daca        --
-- parametrul este par, altfel impar --
---------------------------------------

parImpar x =
    if (length x mod 2 == 0) then "par"
    else "impar"

-----------------------------------
-- 6.3 O functie care calculeaza -- 
-- factorialul unui numar        --
-----------------------------------

factorial x = 
    if (x == 1 || x == 0) then 1
    else x * factorial (x - 1)

--------------------------------------
-- 6.4 O functie care verifica daca --
-- primul parametru este mai mare   --
-- decat dublul  celui de-al doilea --
--------------------------------------

primulParametruMaiMareDecatDublulCeluiDeAlDoileaParametru x y =
    x > 2 * x

-----------------------------------
-- 6.5 O functie care calculeaza --
-- elementul maxim al unui liste --
-----------------------------------

maximLista [x]  = x
maximLista (x : xs) = 
    if x > maximLista xs then x
    else maximLista xs

-----------------------------------
-- 7. Scrieti o functie poly cu  --
-- patru argumente de tip Double --
-- (a, b, c, x) care calculeaza  -- 
-- a * x ^ 2 + b * x + c.        -- 
-- Scrieti si signatura          --
-- functiei poly                 --
-----------------------------------

poly a, b, c, x = 
    a * x * x + b * x + c

-------------------------------------
-- 8. Scrieti o functie eeny care  --
-- intoarce string-ul eeny atunci  --
-- cand primeste ca input un numar --
-- par si meeny cand primeste un   -- 
-- numar impar                     --
-------------------------------------

eeny x =
    if (x mod 2 == 0) then "eeny"
    else "meeny"

------------
-- Others --
------------

-- eeny :: Integer -> String
-- eeny = undefined

-- fizzbuzz :: Integer -> String
-- fizzbuzz = undefined

-- fibonacciCazuri :: Integer -> Integer
-- fibonacciCazuri n
--     | n < 2     = n
--     | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
    
-- fibonacciEcuational :: Integer -> Integer
-- fibonacciEcuational 0 = 0
-- fibonacciEcuational 1 = 1
-- fibonacciEcuational n =
--     fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)
    
-- tribonacci :: Integer -> Integer
-- tribonacci = undefined

-- binomial :: Integer -> Integer -> Integer
-- binomial = undefined
