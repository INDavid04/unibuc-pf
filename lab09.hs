-- 1. Instanțiați clasa următoare pentru tipul Tree.

data Tree = Empty  -- arbore vid
    | Node Int Tree Tree Tree 

class ArbInfo t where
  level :: t -> Int -- intoarce inaltimea arborelui; consideram ca un arbore vid are inaltimea 0
  sumval :: t -> Int -- intoarce suma valorilor din arbore
  nrFrunze :: t -> Int -- intoarce nr de frunze al arborelui

instance ArbInfo Tree where
    level Empty = 0 
    level (Node _ t1 t2 t3) = 1 + maximum [level t1, level t2, level t3]
    
    sumval Empty = 0
    sumval (Node x t1 t2 t3) = x + sumval t1 + sumval t2 + sumval t3
    
    nrFrunze Empty = 0
    nrFrunze (Node _ Empty Empty Empty) = 1
    nrFrunze (Node _ t1 t2 t3) = nrFrunze t1 + nrFrunze t2 + nrFrunze t3

-- 2. Instanțiați clasa Scalar folosindu-vă de tipuri primitive (hint: nu uitați, trebuie să fie corpuri comutative). Apoi, considerați clasa de mai jos a vectorilor.

-- Observatie: Tipul Int nu este corp (Explicatie: 1 / 2 nu este intreg)

class Scalar a where
    zero :: a 
    one :: a 
    adds :: a -> a -> a
    mult :: a -> a -> a
    negates :: a -> a
    recips :: a -> a

instance Scalar Double where
    zero = 0
    one = 1
    adds a b = a + b
    mult a b = a * b
    negates a = -1 * a
    recips a = 1 / a

instance Scalar Float where
    zero = 0
    one = 1
    adds a b = a + b
    mult a b = a * b
    negates a = -1 * a
    recips a = 1 / a

instance Scalar Rational where
    zero = 0
    one = 1
    adds a b = a + b
    mult a b = a * b
    negates a = -1 * a
    recips a = 1 / a

-- 3. Scrieți două instanțe ale clasei Vector pentru a reprezenta vectori bidimensionali și tridimensionali.

data Vec2 a = Vec2 a a
data Vec3 a = Vec3 a a a

class (Scalar a) => Vector v a where
    zerov :: v a
    onev :: v a
    addv :: v a -> v a -> v a -- adunare vector
    smult :: a -> v a -> v a  -- inmultire cu scalare
    negatev :: v a -> v a -- negare vector

instance (Scalar a) => Vector Vec2 a where
    zerov = Vec2 zero zero
    onev = Vec2 one one
    addv (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (adds x1 x2) (adds y1 y2)
    smult s (Vec2 x y) = Vec2 (mult s x) (mult s y)
    negatev (Vec2 x y) = Vec2 (negates x) (negates y)
        
instance (Scalar a) => Vector Vec3 a where
    zerov = Vec3 zero zero zero
    onev = Vec3 one one one
    addv (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (adds x1 x2) (adds y1 y2) (adds z1 z2)
    smult s (Vec3 x y z) = Vec3 (mult s x) (mult s y) (mult s z)
    negatev (Vec3 x y z) = Vec3 (negates x) (negates y) (negates z)
