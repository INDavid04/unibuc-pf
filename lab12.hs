{- 
class Functor f where 
     fmap :: (a -> b) -> f a -> f b 
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

Just length <*> Just "world"

Just (++" world") <*> Just "hello,"
pure (+) <*> Just 3 <*> Just 5
pure (+) <*> Just 3 <*> Nothing
(++) <$> ["ha","heh"] <*> ["?","!"]
-}

-- 1. Se dă tipul de date.

data List a = Nil
    | Cons a (List a)
deriving (Eq, Show)

-- Scrieți instanțe ale claselor Functor și Applicative pentru constructorul de tip List.

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Functie ajutatoare
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    (Cons f fs) <*> vs = append (fmap f vs) (fs <*> vs)

-- Exemple

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

-- 2. Se dă tipul de date

data Dog = Dog {
    name :: String, 
    age :: Int, 
    weight :: Int
} deriving (Eq, Show)

-- 2a. Scrieți funcțiile noEmpty și noNegative care validează un string, respectiv un număr întreg.

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty sir = Just sir

noNegative :: Int -> Maybe Int
noNegative nr
    | nr >= 0 = Just nr
    | otherwise = Nothing

test21 = noEmpty "abc" == Just "abc"
test22 = noNegative (-5) == Nothing 
test23 = noNegative 5 == Just 5 

-- 2b. Scrieți o funcție care construiește un element de tip Dog verificând numele, vârsta și greutatea, folosind funcțiile definite pentru a).

dogFromString :: String -> Int -> Int -> Maybe Dog
dogFromString nume varsta greutate =
    case (noEmpty nume, noNegative varsta, noNegative greutate) of (Just n, Just v, Just g) -> Just (Dog n v g)
    _ -> Nothing

test24 = dogFromString "Toto" 5 11 == Just (Dog {name = "Toto", age = 5, weight = 11})

-- 2c. Scrieți funcția de la b) folosind fmap și <*>

dogFromString :: String -> Int -> Int -> Maybe Dog
dogFromString nume varsta greutate = 
    Dog <$> noEmpty nume <*> noNegative varsta <*> noNegative greutate

-- 3. Se dau următoarele tipuri de date:

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

-- 3a. Implementați o funcție validateLength care validează lungimea unui șir de caractere – să fie mai mică decât numărul dat ca parametru

validateLength :: Int -> String -> Maybe String
validateLength limita sir
    | length sir <= limita = Just sir
    | otherwise = Nothing

test31 = validateLength 5 "abc" == Just "abc"

-- 3b. Implementați funcțiile mkName și mkAddress care transformă un șir de caractere într-un element din tipul de date asociat, validând stringul cu funcția validateLength (numele trebuie să aibă maxim 25 caractere, iar adresa maxim 100)

mkName :: String -> Maybe Name
mkName nume =
    case validateLength 25 nume of
        Just s -> Just (Name s)
        Nothing -> Nothing

mkAddress :: String -> Maybe Address
mkAddress adresa = 
    case validateLength 100 adresa of
        Just s -> Just (Address s)
        Nothing -> Nothing

test32 = mkName "Popescu" ==  Just (Name "Popescu")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

-- 3c. Implementați funcția mkPerson care primește ca argumente două șiruri de caractere și formează un element de tip Person dacă sunt validate condițiile, folosind funcțiile implementate mai sus

mkPerson :: String -> String -> Maybe Person
mkPerson nume adresa = case mkName nume of
    Nothing -> Nothing
    Just n -> case mkAddress adresa of
        Nothing -> Nothing
        Just a -> Just (Person n a)

test34 = mkPerson "Popescu" "Str Academiei" == Just (Person (Name "Popescu") (Address "Str Academiei"))

-- 3d. Implementați funcțiile de la b) și c) folosind fmap și <*>

mkNameUp :: String -> Maybe Name
mkNameUp nume = Name `fmap` validateLength 25 nume

mkAddressUp :: String -> Maybe Address
mkAddressUp adresa = fmap Address (validateLength 100 adresa)

mkPersonUp :: String -> String -> Maybe Person
mkPersonUp nume adresa = (Person `fmap` mkNameUp nume) <*> mkAddressUp adresa
