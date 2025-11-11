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

-- 8. Scrieți o funcție prel2 care are ca parametru o listă de întregi (Int) și întoarce o listă în care elementele pare sunt înjumătățite, iar cele impare sunt dublate:
-- Exemplu: prel2 [2,4,5,6] intoarce [1,2,10,3]

prel2 :: [Int] -> [Int]
prel2 list = [ if x `mod` 2 == 0 then x `div` 2 else x * 2 | x <- list ]

-- 9. Scrieți o funcție care primește ca parametri un caracter și o listă de șiruri de caractere, și întoarce lista șirurilor care conțin caracterul primit ca argument (hint: folosiți funcția elem).

listaSirCar :: Char -> [[Char]] -> [[Char]]
listaSirCar caracter listaSirCaractere = [ sir | sir <- listaSirCaractere, elem caracter sir ]

-- 10. Scrieți o funcție care are ca parametru o listă de întregi și întoarce lista pătratelor numerelor impare din acea listă.

listaPatrateImpare :: [Int] -> [Int]
listaPatrateImpare lista = [ x * x | x <- lista, x `mod` 2 == 1 ]

-- 11. Scrieți o funcție care primește ca argument o listă de întregi și întoarce lista pătratelor elementelor din poziții impare. Hint: folosiți zip pentru a avea acces la poziția elementelor.

listaPatratePozImpare :: [Int] -> [Int]
listaPatratePozImpare lista = [ x * x | (i, x) <- zip [0..] lista, i `mod` 2 == 1 ]

-- 12. Scrieți o funcție care primește ca parametru o listă de șiruri de caractere și întoarce lista obținută prin eliminarea consoanelor din fiecare șir.
-- Exemplu: numaiVocale ["laboratorul", "PrgrAmare", "DEclarativa"] intoarce ["aoaou","Aae","Eaaia"]

numaiVocale :: [[Char]] -> [[Char]]
numaiVocale liste = [ [ caracter | caracter <- lista, caracter `elem` "aeiouAEIOU" ] | lista <- liste ]

-- 13. Definiți recursiv funcțiile mymap și myfilter cu aceeași funcționalitate ca a funcțiilor map și filter predefinite.
-- Exemplu: mymap (* 3) [1, 3, 4] intoarce [3, 9, 12]
-- Exemplu: myfilter (> 2) [3,1,4,2,5] intoarce [3, 4, 5]

-- Din github
-- mymap :: (a -> b) -> [a] -> [b]
-- mymap f xs = [f x | x <- xs]

-- myfilter :: (a -> Bool) -> [a] -> [a]
-- myfilter p xs = [x | x <- xs, p x]

-- Recursiv
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x : xs) = f x : mymap f xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter p (x : xs)
    | p x = x : myfilter p xs
    | otherwise = myfilter p xs
