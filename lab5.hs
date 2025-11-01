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
