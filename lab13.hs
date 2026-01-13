{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing

instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

-- 1. Citiți definițiile de mai jos și încercați să înțelegeți ce face funcția fct. Scrieți apoi o definiție pentru fct folosind notația do.

-- Functia fct ia o valoare de tipul Maybe Int si returneaza un Maybe Bool

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
fct  mx =  mx  >>= (\x -> Just (pos x))

fct_cu_notatia_do :: Maybe Int -> Maybe Bool
fct mx = do
    x <- mx
    return (pos x)

-- 2. Vrem să definim o funcție care adună două valori de tip Maybe Int:

-- a) Definiți addM prin orice metodă (de exemplu, folosind șabloane).

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM (Just x) (Just y) = Just (x + y)
addM _ _ = Nothing 

-- b) Definiți addM folosind operații monadice și notația do
addM2 :: Maybe Int -> Maybe Int -> Maybe Int
addM2 mx my = do
    x <- mx
    y <- my
    return (x + y)

-- 3. Rescrieți următoarele funcții folosind notația do:

cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

cartesian_product_cu_notatia_do :: [a] -> [b] -> [(a, b)]
cartesian_product_cu_notatia_do xs ys = do
    x <- xs
    y <- ys
    return (x, y)

prod f xs ys = [f x y | x <- xs, y<-ys]

prod_cu_notatia_do :: (a -> b -> c) -> [a] -> [b] -> [c]
prod_cu_notatia_do f xs ys = do
    x <- xs
    y <- ys
    return (f x y)

myGetLine :: IO String
myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)

myGetLine_cu_notatia_do :: IO String
myGetLine_cu_notatia_do = do
    x <- getChar
    if x == '\n'
        then return []
    else do
        xs <- myGetLine
        return (x:xs)

-- 4. Rescrieți următoarea funcție folosind notația cu secvențiere:

prelNo noin =  sqrt noin

ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout

ioNumber_cu_secventiere :: IO ()
ioNumber_cu_secventiere =
    (readLn :: IO Float) >>= \noin ->
        putStrLn("Intrare\n" ++ show noin) >>
            let noout = prelNo noin in
                putStrLn "Iesire" >>
                    print noout

-- 5. Pentru următoarele exerciții veți folosi fișierul mWriter.hs, ce conține o definiție a monadei Writer String (modificată pentru a compila fără opțiuni suplimentare):

--- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } 

-- a. Definiți funcțiile logIncrement și logIncrement2 din curs și testați-le.

instance  Monad WriterS where
    return va = Writer (va, "")
    ma >>= k = let (va, log1) = runWriter ma
                    (vb, log2) = runWriter (k va)
                in  Writer (vb, log1 ++ log2)

instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)
  
logIncrement :: Int  -> WriterS Int
logIncrement x = do
    tell ("Increment: " ++ show x ++ "; ")
    return (x + 1)

logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do
    y <- logIncrement x
    logIncrement y

-- b. Definiți funcția logIncrementN, care generalizează logIncrement2, astfel:

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n =
    if n <= 0
        then return x
    else do
        y <- logIncrement x
        logIncrementN y (n - 1)

runWriter $ logIncrementN 2 4
(6,"increment:2\nincrement:3\nincrement:4\nincrement:5\n")

-- c. Modificați definiția monadei WriterS astfel încât să producă lista mesajelor de log și nu concatenarea lor. Pentru a evita posibile confuzii, lucrați în alt fișier. Definiți funcția logIncrementN în acest context.

newtype WriterLS a = Writer {runWriter :: (a, [String])}

instance Monad WriterLS where
    return va = Writer (va, [])
    ma >>= k = let (va, log1) = runWriter ma
                    (vb, log2) = runWriter (k va)
                in Writer (vb, log1 ++ log2)

instance Applicative WriterLS where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)

instance Functor WriterLS where
    fmap f ma = pure f <*> ma

tell :: String -> WriterLS ()
tell log = Writer ((), [log])

logIncrement10 :: Int -> WriterLS Int
logIncrement10 x = do
    tell ("increment: " ++ show x)
    return (x + 1)

logIncrementN10 :: Int -> Int -> WriterLS Int
logIncrementN10 x n =
    if n <= 0 then
        return x
    else do
        y <- logIncrement10 x
        logIncrementN10 y (n - 1)

runWriter $ logIncrementN 2 4
(6,["increment:2","increment:3","increment:4","increment:5"])

-- 6. Definim tipul de date:

data Person = Person { name :: String, age :: Int }

-- a. Definiți funcțiile

showPersonN :: Person -> String
showPersonN p = "NAME: " ++ name p

showPersonA :: Person -> String
showPersonA p = "AGE: " ++ show (age p) -- show pentru ca age e de tipul Int

{-
showPersonN $ Person "ada" 20
"NAME: ada"
showPersonA $ Person "ada" 20
"AGE: 20"
-}

-- b. Folosind funcțiile definite pentru exercițiile 5.a) și 5.b), definiți funcția

showPerson :: Person -> String
showPerson p = "(" ++ showPersonN p ++ ", " ++ showPersonA p ++ ")"

{-
showPerson $ Person "ada" 20
"(NAME: ada, AGE: 20)"
-}

-- c. Folosind monada Reader (găsiți implementarea instanțelor în fișierul lab13.hs), definiți variante monadice pentru cele trei funcții definite anterior. Variantele monadice vor avea tipul:

ask = Reader (\p -> p)

mshowPersonN ::  Reader Person String
mshowPersonN = do
    p <- ask
    return ("NAME: " ++ name p)

mshowPersonA ::  Reader Person String
mshowPersonA = do
    p <- ask
    return ("AGE: " ++ show (age p)) 

mshowPerson ::  Reader Person String
mshowPerson = do
    n <- mshowPersonN
    a <- mshowPersonA
    return ("(" ++ n ++ ", " ++ a ++ ")")
{-
runReader mshowPersonN  $ Person "ada" 20
"NAME:ada"
runReader mshowPersonA  $ Person "ada" 20
"AGE:20"
runReader mshowPerson  $ Person "ada" 20
"(NAME:ada,AGE:20)"
-}
