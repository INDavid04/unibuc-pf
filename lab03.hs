import Data.Char -- pentru problema 10 :)

------------------------------------------------------
-- 1. Implementati urmatoare functii folosind liste --
------------------------------------------------------

-- 1.a. Verifica daca lungimea unei liste este para (verifL)

verifL :: [Int] -> Bool
verifL lista = 
    if ((length lista) `mod` 2 == 0) then True
    else False

-- 1.b. Pentru o lista l data ca parametru si un numar n, intoarce o lista care contine ultimele n elemente ale listei l.

takefinal :: [Int] -> Int -> [Int]
takefinal lista n = 
    drop (length lista - n) lista

-- Cum trebuie să modificăm prototipul funcției pentru a putea folosi funcția și pentru șiruri de caractere?

-- takefinal :: [a] -> Int -> [a]

-- c) Pentru o listă și un număr n, întoarce lista primită ca parametru din care se șterge elementul de pe poziția n. (Hint: puteți folosi funcțiile take și drop). Scrieți și prototipul funcției.

remove :: [a] -> Int -> [a]
remove lista n =
    take n lista ++ drop (n + 1) lista

-- Explicatie: ++ inseamna concatenare, take n lista (adica de la 0 la n - 1) si drop (n + 1) lista, adica restul

-------------------------------------------------------------------------
-- 2. Scrieți următoarele funcții folosind conceptul de recursivitate: --
-------------------------------------------------------------------------

-- a) myreplicate - pentru un întreg n și o valoare v, întoarce lista ce conține n elemente egale cu v. Să se scrie și prototipul funcției.

myreplicate :: Int -> a -> [a]
myreplicate 0 v = []
myreplicate n v =
    v : myreplicate (n - 1) v

-- Explicatie: Operatorul : adauga un element in fata unei liste
     
-- b) sumImp - pentru o listă de numere întregi, calculează suma elementelor impare. Să se scrie și prototipul funcției.

sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (item : list)
    | odd item = item + sumImp list
    | otherwise = sumImp list

-- c) totalLen - pentru o listă de șiruri de caractere, calculează suma lungimilor șirurilor care încep cu caracterul 'A'.

totalLen :: [String] -> Int
totalLen [] = 0
totalLen (sir : siruri)
    | head sir == 'A' = length sir + totalLen siruri
    | otherwise = totalLen siruri

-- 3. Scrieți o funcție nrVocale care primește ca parametru o listă de șiruri de caractere și calculează numărul total de vocale din șirurile palindrom. Pentru a verifica dacă un șir e palindrom, puteți folosi funcția reverse, iar pentru a căuta un element într-o listă, puteți folosi funcția elem. Puteți defini funcții auxiliare.
-- Exemplu: nrVocale ["sos", "civic", "palton", "desen", "aerisirea"] == 9

nrVocaleSir :: String -> Int
nrVocaleSir [] = 0
nrVocaleSir (caracter : sir) 
    | caracter `elem` "aeiouAEIOU" = 1 + nrVocaleSir sir
    | otherwise = nrVocaleSir sir

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (sir : siruri)
    | sir == reverse sir = nrVocaleSir sir + nrVocale siruri
    | otherwise = nrVocale siruri

-- 4. Scrieți o funcție care primește ca parametri un număr și o listă de întregi și adaugă numărul dat după fiecare element par din listă. Să se scrie și prototipul funcției.
-- Exemplu: f 3 [1,2,3,4,5,6] == [1,2,3,3,4,3,5,6,3]

f4 :: Int -> [Int] -> [Int]
f4 n [] = []
f4 n (item : list) 
    | item `mod` 2 == 0 = item : n : f4 n list
    | otherwise = item : f4 n list

-- 5. Scrieți o funcție care determină lista divizorilor unui număr întreg primit ca parametru. Să se scrie și prototipul funcției.

eDivizor :: Int -> Int -> Bool
eDivizor x n =
    if (n `mod` x == 0) then True
    else False

divizori :: Int -> [Int]
divizori n = [ x | x <- [1..n], eDivizor x n ]

-- 6. Scrieți o funcție care primește ca parametru o listă de numere întregi și întoarce lista listelor de divizori.
-- Exemplu: listadiv [1,4,6,8] == [[1],[1,2,4],[1,2,3,6],[1,2,4,8]]

listadiv :: [Int] -> [[Int]]
listadiv lista = [ divizori x | x <- lista ]

-- 7. Scrieți o funcție care primește ca parametri: două numere întregi ce reprezintă limita inferioară și cea superioară a unui interval închis și o listă de numere întregi și întoarce numerele din listă ce aparțin intervalului. 
-- Exemplu: inInterval 5 10 [1..15] == [5,6,7,8,9,10]
-- Exemplu: inInterval 5 10 [1,3,5,2,8,-1] == [5,8]

-- a) Definiți funcția recursiv și denumiți-o inIntervalRec. 

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b [] = []
inIntervalRec a b (item : list) 
    | item >= a && item <= b = item : inIntervalRec a b list
    | otherwise = inIntervalRec a b list

-- b) Folosiți descrieri de liste. Denumiți funcția inIntervalComp.

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b lista = [ x | x <- lista, x >= a, x <= b ]

-- 8. Scrieți o funcție care numără câte numere strict pozitive sunt într-o listă dată ca argument.
--  Hint: Nu puteți folosi recursivitate. Veți avea nevoie de o funcție de agregare (consultați modulul [`Data.List`](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-List.html)). De ce nu e posibil să scriem `pozitiveComp` folosind doar descrieri de liste?
-- Exemplu: pozitive [0,1,-3,-2,8,-1,6] == 3

-- a) Definiți funcția recursiv și denumiți-o pozitiveRec.
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (item : list)
    | item > 0 = 1 + pozitiveRec list
    | otherwise = pozitiveRec list 

-- b) Folosiți descrieri de liste. Denumiți funcția pozitiveComp.
pozitiveComp :: [Int] -> Int
pozitiveComp list = length [item | item <- list, item > 0]

-- 9. Scrieți o funcție care întoarce lista pozițiilor elementelor impare dintr-o listă de numere primită ca parmetru. 
-- De exemplu: pozitiiImpare [0,1,-3,-2,8,-1,6,1] == [1,2,5,7]

-- a) Definiți funcția recursiv și denumiți-o pozitiiImpareRec.
-- Hint: Folosiți o funcție ajutătoare, cu un parametru în plus reprezentând poziția curentă din listă.

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec [] = []
pozitiiImpareRec list = f list 0
    where
        f [] _ = []
        f (item : list) position
            | item `mod` 2 == 1 = position : f list (position + 1)
            | otherwise = f list (position + 1)

-- b) Folosiți descrieri de liste. Denumiți funcția pozitiiImpareComp
-- Hint: folosiți funcția `zip` pentru a asocia poziții elementelor listei (puteți găsi un exemplu în curs)

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp list = [position | (item, position) <- zip list [0..], item `mod` 2 == 1]

-- 10. Scrieți o funcție care calculează produsul tuturor cifrelor care apar într-un șir de caractere primit ca parametru. Dacă șirul nu conține cifre, funcția întoarce 1.
-- De exemplu: multDigits "The time is 4:25" == 40 si multDigits "No digits here!" == 1
-- Hint: Veți avea nevoie de funcția `isDigit` care verifică dacă un caracter e cifră și de funcția `digitToInt` care transformă un caracter în cifră. Cele 2 funcții se află în pachetul `Data.Char`.

-- a) Definiți funcția recursiv și denumiți-o multDigitsRec.
multDigitsRec :: [Char] -> Int
multDigitsRec [] = 1
multDigitsRec (character : characters)
    | isDigit character = digitToInt character * multDigitsRec characters
    | otherwise = multDigitsRec characters

-- b) Folosiți descrieri de liste. Denumiți funcția multDigitsComp.
multDigitsComp :: [Char] -> Int
multDigitsComp characters = product [digitToInt character| character <- characters, isDigit character]
