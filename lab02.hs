-----------
-- Intro --
-----------

import Data.List

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

-- Tine mine: Putem folosi modulo in doua moduri:
-- 1. Ca operator: x `mod` 2 == 0
-- 2. Ca functie: mod x 2 == 0

parImpar x =
    if (length x `mod` 2 == 0) then "par"
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

poly a b c x = 
    a * x * x + b * x + c

-------------------------------------
-- 8. Scrieti o functie eeny care  --
-- intoarce string-ul eeny atunci  --
-- cand primeste ca input un numar --
-- par si meeny cand primeste un   -- 
-- numar impar                     --
-------------------------------------

eeny x =
    if (mod x 2 == 0) then "eeny"
    else "meeny"

--------------------------------------
-- 9. Scrie o functie fizzbuzz.     --
-- Ea intoarce "Fizz" pentru numere --
-- divizibile cu 3, "Buzz" pentru   --
-- numere divizibile cu 5 si        --
-- "FizzBuzz" pentru numere         --
-- divizibile cu ambele.            --
-- Pentru orice alt nuamr intoarce  --
-- sirul vid.                       --
--------------------------------------

fizzbuzz x =
    if (x `mod` 3 == 0 && x `mod` 5 == 0) then "FizzBuzz"
    else if (mod x 3 == 0) then "Fizz"
    else if (mod x 5 == 0) then "Buzz"
    else ""

--------------------------------------
-- 10. Scrieti functia tribonacci = --
-- 1 : n = 1                        --
-- 1 : n = 2                        --
-- 2 : n = 3                        --
-- t(n-1) + t(n-2) + t(n-3) : n > 3 --
--------------------------------------

tribonacci :: Integer -> Integer
tribonacci n
    | n == 1    = 1
    | n == 2    = 1
    | n == 3    = 2
    | otherwise = tribonacci(n - 1) + tribonacci(n - 2) + tribonacci(n - 3)

--------------------------------------
-- 11. Scrie o functie care         --
-- calculeaza coeficientii          --
-- binomiali. Avem regula B(n, k) = --
-- B(n-1,k) + B(n-1,k-1),           --
-- B(n, 0) = 1,                     --
-- B(0, k) = 0.                     --
--------------------------------------

-- Tine minte: binomial(n-1, k) vine scris in felul: binomial (n - 1) k

binomial :: Integer -> Integer -> Integer
binomial n k
    | k == 0    = 1
    | n == 0    = 0
    | otherwise = binomial (n - 1) (k) + binomial (n - 1) (k - 1)
