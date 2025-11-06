-- 1. Calculați suma pătratelor elementelor impare dintr-o listă dată ca parametru.
sumaImpare :: [Int] -> Int
sumaImpare list = sum [x * x | x <- list, x `mod` 2 == 1]

-- 2. Scrieți o funcție care verifică că toate elementele dintr-o listă sunt True, folosind foldr
toateAdevarate :: [Bool] -> Bool
toateAdevarate list = foldr (&&) True [ x | x <- list ]

-- 3. Scrieți o funcție care verifică dacă toate elementele dintr-o listă de numere întregi satisfac o proprietate dată ca parametru.
allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies f list = foldr (\x res -> f x && res) True list
-- Bonus: in loc de lambda expression: \x res -> f x && res puteam scrie
-- allVerifies f list = foldr aux True list
--     where
--         aux x res = f x && res

-- 4. Scrieți o funcție care verifică dacă există elemente într-o listă de numere întregi care satisfac o proprietate dată ca parametru.
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies f list = foldr (\x res -> f x || res) False list
-- Bonus: In loc de lambda expression \x res -> f x || res puteam scrie
-- anyVerifies f list = foldr aux False list
--     where
--         aux x res  = f x || res

-- 5. Redefiniți funcțiile map și filter folosind foldr. Le puteți numi mapFoldr și filterFoldr.

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f xs = foldr (\x acc -> f x : acc) [] xs

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p xs = foldr (\x acc -> if p x then (x : acc) else acc) [] xs

-- 6. Folosind funcția foldl, definiți funcția listToInt care transformă o listă de cifre (un număr foarte mare reprezentat ca listă) în numărul întreg asociat. Se presupune că lista de intrare este dată corect.
-- Exemplu: listToInt [2,3,4,5] = 2345
listToInt :: [Integer] -> Integer
listToInt list = foldl (\acc x -> acc * 10 + x) 0 list


-- 7a. Scrieți o funcție care elimină toată aparițiile unui caracter dat dintr-un șir de caractere.
rmChar :: Char -> String -> String
rmChar caracter sirDeCaractere = foldl (\acc x -> if x /= caracter then acc ++ [x] else acc) "" sirDeCaractere 

-- 7b. Scrieți o funcție recursivă care elimină toate caracterele din al doilea argument care se găsesc în primul argument, folosind rmChar.
rmCharsRec :: String -> String -> String
rmCharsRec [] sir2 = sir2
rmCharsRec (caracter:sir1) sir2 = rmCharsRec sir1 (rmChar caracter sir2)

-- 8. Scrieți o funcție myReverse care primește ca parametru o listă de întregi și întoarce lista elementelor în ordine inversă.
myReverse :: [Int] -> [Int]
myReverse list = foldl (\acc x -> x : acc) [] list

-- 9. Scrieți un predicat myElem care verifică apartenența unui întreg la o listă de întregi.
myElem :: Int -> [Int] -> Bool
myElem val list = foldl (\acc x -> acc || x == val) False list

-- 10. Scrieți o funcție myUnzip care transformă o listă de perechi într-o pereche de liste: una a componentelor de pe prima poziție, iar cealaltă a componentelor de pe a doua poziție din perechile din lista inițială.
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip list = foldl (\(acc1, acc2) (x, y) -> (acc1 ++ [x], acc2 ++ [y])) ([], []) list

-- 11. Scrieți o funcție union care întoarce lista reuniunii a două liste de întregi primite ca parametri.
union :: [Int] -> [Int] -> [Int]
union l1 l2 = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) l1 l2

-- 12. Scrieți o funcție intersect care întoarce lista intersecției a două liste de întregi primite ca parametri.
intersect :: [Int] -> [Int] -> [Int]
intersect l1 l2 = foldl (\acc x -> if x `elem` l1 && not (x `elem` acc) then acc ++ [x] else acc) [] l2

-- 13. Scrieți o funcție permutations care întoarce lista tuturor permutărilor elementelor unei liste de întregi primite ca parametru.
permutations :: [Int] -> [[Int]]
permutations = foldr (\x acc -> concatMap (insertElem x) acc) [[]] where
    insertElem :: Int -> [Int] -> [[Int]]
    insertElem x [] = [[x]]
    insertElem x (y:ys) = (x:y:ys) : map (y:) (insertElem x ys)
