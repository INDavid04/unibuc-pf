-- 1. Adăugați definiții implicite (folosind celelalte funcții din clasă) pentru keys, values și fromList

class Collection c where
    empty :: c key value
    singleton :: key -> value -> c key value
    insert
        :: Ord key
        => key -> value -> c key value -> c key value
    lookup :: Ord key => key -> c key value -> Maybe value
    delete :: Ord key => key -> c key value -> c key value
    toList :: c key value -> [(key, value)]

    -- Extrage prima componeneta din fiecare pereche toList
    keys :: c key value -> [key]
    keys = map fst . toList 

    -- Extrage a doua componenta din fiecare pereche toList
    values :: c key value -> [value]
    values = map snd . toList

    -- Construieste o colectie inserand perechile din lista
    fromList :: Ord key => [(key,value)] -> c key value
    fromList = foldl (\acc (k, v) -> insert k v acc) empty

-- 2. Fie tipul listelor de perechi cheie-valoare. Faceți PairList instanță a clasei Collection

newtype PairList k v = PairList { getPairList :: [(k, v)] }
    deriving Show -- pentru a putea afisa valorile in ghci

instance Collection PairList where
    empty = PairList[]
    singleton k v = PairList[(k, v)]
    insert k v (PairList xs) = PairList((k, v) : xs)
    lookup k (PairList xs) = Prelude.lookup k xs
    delete k (PairList xs) = PairList (filter (\(k', _) -> k /= k') xs)
    toList (PairList xs) = xs
    -- Mosteneste definitiile implicite pentru kyes, values, fromList

-- 3. Amintiți-vă exercițiul din laboratorul trecut în care ați definit tipul arborilor de căutare cu noduri constând în perechi chei-valoare cu chei numere întregi. Vom generaliza acest tip definind arbori binari de căutare (ne-echilibrați) cu chei de tip oarecare. Observați că tipul valorilor este Maybe value. Alegerea a fost făcută pentru a reduce timpul operației de ștergere prin simpla marcare a unui nod ca fiind șters. Un nod șters va avea valoarea Nothing. Faceți SearchTree instanță a clasei Collection

data SearchTree key value
    = Empty
    | BNode
        (SearchTree key value) -- elemente cu cheia mai mica
        key                    -- cheia elementului
        (Maybe value)          -- valoarea elementului
        (SearchTree key value) -- elemente cu cheia mai mare
    deriving Show

instance Collection SearchTree where
    empty = Empty
    singleton k v = BNode Empty k (Just v) Empty

    insert k v Empty = BNode Empty k (Just v) Empty
    insert k v (Bnode left key val right)
        | k < key = BNode (insert k v left) key val right
        | k > key = BNode left key val (insert k v right)
        | k == key = BNode left key (Just v) right -- se suprascrie

    lookup _ Empty = Nothing
    lookup k (BNode left key val right)
        | k < key = lookup k left
        | k > key = lookup k right
        | k == key = val

    delete _ Empty = Empty
    delete k (BNode left key val right)
        | k < key = BNode (delete k left) key val right
        | k > key = BNode left key val (delete k right)
        | otherwise = BNode left key Nothing right -- marcat ca sters

    toList Empty = []
    toList (BNode left key val right) = 
        toList left
        ++ case val of
            Just v -> [(key, v)]
            Nothing -> []
        ++ toList right

-- 4. Scrieți o instanță a clasei Show pentru tipul de date Punct, astfel încât lista coordonatelor să fie afișată ca tuplu.
-- Pt [1,2,3]
-- (1, 2, 3)
-- Pt []
-- ()

data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance Show Punct where
    show (Pt xs) = "(" ++ afis xs ++ ")" where
        afis [] = ""
        afis [x] = show x
        afis (x:xs) = show x ++ ", " ++ afis xs

-- 5. Scrieți o instanță a clasei ToFromArb pentru tipul de date Punct astfel încât lista coordonatelor punctului să coincidă cu frontiera arborelui.
-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
-- (1,2,3)

instance ToFromArb Punct where
    toArb (Pt xs) = foldr inserare Vid xs wehere
        inserare x acc = N (F x) acc
    fromArb arb = Pt (fr arb) where
        fr Vid = []
        fr (F x) = [x]
        fr (N st dr) = fr st ++ fr dr

-- 6. Instanțiați clasa GeoOps pentru tipul de date Geo. Hint: pentru valoarea pi puteți folosi funcția cu același nume (pi).
-- ghci> pi
-- 3.141592653589793

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
    perimeter :: (Floating a) => g a -> a
    area :: (Floating a) =>  g a -> a

instance GeoOps Geo where
    perimeter (Square l) = 4 * l
    perimeter (Rectangle l1 l2) = 2 * (l1 + l2) 
    perimeter (Circle r) = 2 * pi * r

-- 7. Instanțiați clasa Eq pentru tipul de date Geo, astfel încât două figuri geometrice să fie egale dacă au perimetrul egal.

instance (Floating a, Eq a) => Eq (Geo a) where
    g1 == g2 = perimeter g1 == perimeter g2
