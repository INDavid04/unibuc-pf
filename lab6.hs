-- 1. Vom incepe prin a scrie cateva functii folosind tipul de date Fruct
data Fruct = Mar String Bool | Portocala String Int

-- Explicatie: String indica soiul de mere sau de portocale
-- Explicatie: Bool indica daca marul are viermi
-- Explicatie: Int indica numarul de felii dintr-o portocala

-- Exemplu: ionatanFaraVierme = Mar "Ionatam" False
-- Exemplu: goldenCuViermie = Mar "Golden Delicious" True
-- Exemplu: portocalaSilicia10 = Portocala "Sanguinello" 10
-- Exemplu: cosFructe = [Mar "Ionatam" False, Portocala "Sanguinello" 10, Mar "Golden" True]

-- 1a. Scrieți un predicat care verifică dacă un fruct este o portocală de Sicilia. Soiurile de portocale din Sicilia sunt Tarocco, Moro și Sanguinello
-- Exemplu: ePortocalaDeSicilia (Portocala "Moro" 12) == True
-- Exemplu: ePortocalaDeSicilia (Mar "Ionatan" True) == False
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala soi _) = soi == "Tarocco" || soi == "Moro" || soi == "Sanguinello"
ePortocalaDeSicilia _ = False

-- 1b. Scrieți o funcție care calculează numărul total de felii ale portocalelor de Sicilia dintr-o listă de fructe.
-- in: nrFeliiSicilia [Mar "Ionatan" False, Portocala "Sanguinello" 10, Mar "Golden" True, Portocala "Moro" 8, Portocala "Valencia" 12] ... out: afiseaza 18

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (Portocala soi nrFelii : rest)
    | soi == "Tarocco" || soi == "Moro" || soi == "Sanguinello" = nrFelii + nrFeliiSicilia rest
    | otherwise = nrFeliiSicilia rest
nrFeliiSicilia (_ : rest) = nrFeliiSicilia rest

-- 1c. Scrieți o funcție care calculează numărul de mere care au viermi dintr-o listă de fructe.
-- Exemplu: in: nrMereViermi [Mar "Ionatan" False, Portocala "Sanguinello" 10, Mar "Golden" True, Mar "Fuji" True, Portocala "Moro" 8] ... out: 2

nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi (Mar _ True : rest) = 1 + nrMereViermi rest
nrMereViermi (Mar _ False : rest) = nrMereViermi rest
nrMereViermi (_ : rest) = nrMereViermi rest

-- 2. Se da tipul de date Animal
type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

-- 2a. Scrieți o funcție care întoarce "Meow!" pentru pisică și "Woof!" pentru câine.
-- Exemplu: vorbeste (Pisica "Yuki") ... "Meow!"
-- Exemplu: vorbeste (Caine "Bella" "bichon") ... "Woof!"

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

-- 2b. Reamintiți-vă tipul de date predefinit Maybe. Scrieți o funcție care întoarce rasa unui câine dat ca parametru sau Nothing dacă parametrul este o pisică.
-- Exemplu: rasa (Caine "Rex" "Labrador") ... Labrador
-- Exemplu: rasa (Pisica "Siamese") ... Nothing

-- data Maybe a = Nothing | Just a

rasa :: Animal -> Maybe String
rasa (Caine _ rasa) = Just rasa
rasa (Pisica _) = Nothing

-- 3. Se dau următoarele tipuri de date ce reprezintă matrici cu linii de lungimi diferite:
data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

-- 3.a Scrieți o funcție care verifică dacă suma elementelor de pe fiecare linie este egală cu o valoare dată n. Rezolvați cerința folosind foldr.
-- Exemplu: verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 ... False
-- Exemplu: verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 ... True

verifica :: Matrice -> Int -> Bool
verifica (M linii) n = foldr (\(L xs) acc -> sum xs == n && acc) True linii

-- 3b. Scrieți o funcție doarPozN care are ca parametri un element de tip Matrice și un număr întreg n, și care verifică dacă toate liniile de lungime n din matrice au numai elemente strict pozitive.
-- Exemplu: doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 ... True
-- Exemplu: doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 ... False

toatePozitive :: [Int] -> Bool
toatePozitive [] = True
toatePozitive (element : linie)
    | element > 0 = toatePozitive linie
    | otherwise = False

doarPozN :: Matrice -> Int -> Bool
doarPozN (M []) _ = True
doarPozN (M (L xs : rest)) n
    | length xs == n = toatePozitive xs && doarPozN (M rest) n
    | otherwise = doarPozN (M rest) n

-- 3.c Definiți predicatul corect care verifică dacă toate liniile dintr-o matrice au aceeași lungime.
-- Exemplu: corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) ... False
-- Exemplu: corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) ... True

corect :: Matrice -> Bool
corect (M []) = True -- matricea vida
corect (M [L _]) = True -- matricea cu o singura linie
corect (M(L xs : L ys : rest)) = length xs == length ys && corect(M(L ys : rest))