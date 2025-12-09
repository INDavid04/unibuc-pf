-- 1. Scrieți o funcție evalExp :: Expr -> Int care evaluează o expresie determinând valoarea acesteia.

data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)

instance Show Expr where
    show :: Expr -> String
    show (Const x) = show x
    show (e1 :+: e2) = "(" ++ show e1 ++ " + "++ show e2 ++ ")"
    show (e1 :*: e2) = "(" ++ show e1 ++ " * "++ show e2 ++ ")"

-- in: evalExp ((Const 1 :+: Const 4) :*: (Const 10)) ... out: 50
evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (e1 :+: e2) = evalExp e1 + evalExp e2
evalExp (e1 :*: e2) = evalExp e1 * evalExp e2

-- 2. Scrieți o funcție evalArb :: Tree -> Int care evaluează o expresie modelată sub formă de arbore, determinând valoarea acesteia.
-- in: evalArb arb3 = 13

arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

-- test21 = evalArb arb1 == 6
-- test22 = evalArb arb2 == 14
-- test23 = evalArb arb3 == 13
-- test24 = evalArb arb4 == 16

evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add t1 t2) = evalArb t1 + evalArb t2
evalArb (Node Mult t1 t2) = evalArb t1 * evalArb t2

-- 3. Scrieți o funcție expToArb :: Expr -> Tree care transformă o expresie în arborele corespunzător.
-- in: (Const 1 :+: Const 2) :*: Const 3 ... out: Node Mult (Node Add (Lf 1) (Lf 2)) (Lf 4)
expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (e1 :+: e2) = Node Add (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2)

-- 4. Scrieți o funcție lookup' de căutare a unui element într-un arbore.

-- in: lookup' 10 arb11 ... Just A

data IntSearchTree value
    = Empty
    | BNode
        (IntSearchTree value)     -- elemente cu cheia mai mica
        Int                       -- cheia elementului
        (Maybe value)             -- valoarea elementului
        (IntSearchTree value)     -- elemente cu cheia mai mare
    deriving Show

arb11 :: IntSearchTree String
arb11 =
  BNode
    (BNode Empty 5 (Just "B") Empty)
    10
    (Just "A")
    (BNode Empty 15 (Just "C") Empty)

lookup' :: Int -> IntSearchTree value -> Maybe value
lookup' _ Empty = Nothing
lookup' key (BNode left k v right)
    | key == k = v
    | key < k = lookup' key left
    | key > k = lookup' key right

-- 5. Scrieți o funcție care întoarce lista cheilor nodurilor dintr-un arbore de căutare.
-- in: keys arb11 ... out: [5, 10, 15]

keys ::  IntSearchTree value -> [Int]
keys Empty = []
keys (BNode left k _ right) = keys left++ [k] ++ keys right

-- 6. Scrieți o funcție care întoarce lista valorilor nodurilor dintr-un arbore de căutare.
values :: IntSearchTree value -> [value]
-- in: values arb11 ... out: ["B","A","C"]

values Empty = []
values (BNode left _ v right) = values left++ maybeToList v ++ values right where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

-- 7. Scrieți o funcție de adăugare a unui element într-un arbore de căutare.
-- in: insert 10 "D" arb11 ... out: BNode (BNode Empty 5 (Just "B") Empty) 10 (Just "D") (BNode Empty 15 (Just "C") Empty)

insert :: Int -> value -> IntSearchTree value -> IntSearchTree value
insert key value Empty = BNode Empty key (Just value) Empty
insert key value (BNode left k v right)
    | key == k = BNode left k (Just value) right
    | key < k = BNode (insert key value left) k v right
    | key > k = BNode left k v (insert key value right)

-- 8. Scrieți o funcție care șterge (marchează ca șters) un element dintr-un arbore de căutare.
-- in: let arb12 = delete 10 arb11; in: values arb12 ... out: ["B","C"]

delete :: Int -> IntSearchTree value -> IntSearchTree value
delete _ Empty = Empty
delete key (BNode left k v right)
    | key == k = BNode left k Nothing right
    | key < k = BNode (delete key left) k v right
    | key > k = BNode left k v (delete key right)

-- 9. Scrieți o funcție care întoarce lista elementelor dintr-un arbore de căutare. Hint: atenție la Maybe!
-- in: toList arb11 ... out: [(5,"B"),(10,"A"),(15,"C")]

toList :: IntSearchTree value -> [(Int, value)]
toList Empty = []
toList (BNode left k v right) = 
    toList left ++ maybeToList k v ++ toList right where
        maybeToList _ Nothing = []
        maybeToList k (Just x) = [(k, x)]

-- 10. Scrieți o funcție care să construiască un arbore dintr-o listă de perechi cheie-valoare.
-- in: fromList [(10,"A"), (5,"B"), (15,"C")] ... out: BNode (BNode Empty 5 (Just "B") Empty) 10 (Just "A") (BNode Empty 15 (Just "C") Empty)

fromList :: [(Int, value)] -> IntSearchTree value 
fromList = foldl (\tree (k, v) -> insert k v tree) Empty

-- 11. Scrieți o funcție care să producă o reprezentare liniară (șir de caractere) a structurii arborescente de chei (ignorând valorile). De exemplu, arborele cu rădăcina cu cheia 2, copilul stâng cu cheia 1 și copilul drept cu cheia 3 ar putea fi reprezentat ca "(1) 2 (3)". Puteți alege și alte reprezentări.
-- in: printTree arb11 ... out: "(()5())10(()15())"

printTree :: IntSearchTree value -> String
printTree Empty = ""
printTree (BNode left k _ right) = "(" ++ printTree left ++ ")" ++ show k ++ "(" ++ printTree right ++ ")"
