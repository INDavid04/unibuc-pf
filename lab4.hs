-- 1. Reamintiți-vă definirea listelor prin selecție din Laboratorul 3. Încercați să aflați valoarea expresiilor de mai jos (fără a folosi interpretorul), iar apoi verificați-vă răspunsurile folosind ghci.
{-
    [ x^2 |x <- [1..10], x `rem` 3 == 2 ] ---- 4,25,64
    [ (x,y) | x <- [1..5], y <- [x..(x+2)] ] ---- (1,1), (1,2), (1,3), (2,2), (2,3), ...
    [ (x,y) | x <- [1..3], let k = x^2, y <- [1..k] ] ---- (1,1), (2,1), (2,2), (2,3), (2,4), (3,1), (3,2), (3,3), (3,4), (3,5), (3,6), (3,7), (3,8), (3,9)
    [ x | x <- "Facultatea de Matematica si Informatica", elem x ['A'..'Z'] ] ---- [F,M,I]
    [ [x..y] | x <- [1..5], y <- [1..5], x < y ] ---- [[1,2], [1,2,3], [1,2,3,4], [1,2,3,4,5], [2,3], [2,3,4], [2,3,4,5], [3,4], [3,4,5], [4,5]]
-}

-- 2. Definiți o funcție factori care întoarce lista divizorilor pozitivi ai unui număr primit ca parametru. Folosiți doar metoda de definire a listelor prin selecție.
eDivizor :: Int -> Int -> Bool
eDivizor x n =
    if (n `mod` x == 0) then True
    else False
factori :: Int -> [Int]
factori n = [x | x <- [1..n], eDivizor x n]

-- 3. Folosind funcția factori, definiți predicatul prim, care verifică dacă un număr primit ca parametru este prim.
prim :: Int -> Bool
prim n =
    if factori n == [1,n] then True
    else False

-- 4. Definiți funcția numerePrime, care pentru un număr n primit ca parametru, întoarce lista numerelor prime din intervalul [2..n]. Folosiți metoda de definire a listelor prin selecție și funcțiile definite anterior.
numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x]

-- 5. Definiți funcția myzip3 ca o generalizare a funcției zip pentru trei argumente:
-- Exemplu: myzip3 [1,2,3] [1,2] [1,2,3,4] == [(1,1,1),(2,2,2)]
myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 (item1 : list1) (item2 : list2) (item3 : list3) =
    (item1, item2, item3) : myzip3 list1 list2 list3 
myzip3 _ _ _ = []

-- 6. Scrieți o funcție generică firstEl care primește ca parametru o listă de perechi de tip (a,b) și întoarce lista primelor elementelor din fiecare pereche:
-- Exemplu: firstEl [('a',3),('b',2), ('c',1)] intoarce "abc"
firstEl :: [(a, b)] -> [a]
firstEl list = [x | (x, _) <- list]

-- 7. Scrieți funcția sumList care are ca parametru o listă de liste de valori Int și întoarce lista sumelor elementelor din fiecare listă (suma elementelor unei liste de întregi se calculează cu funcția sum):
-- Exemplu: sumList [[1,3], [2,4,5], [], [1,3,5,6]] intoarce [4,11,0,15]
sumList :: [[Int]] -> [Int]
sumList lists = [sum list | list <- lists]
