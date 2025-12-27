import Data.List (nub)
import Data.Maybe (fromJust)

-- 1. Scrieți următoarele formule ca expresii de tip Prop, denumindu-le pa, pb, pc.

type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop

  -- Pentru Exercitiul 8. Extindeți tipul de date Prop și funcțiile definite până acum pentru a include conectorii logici
  | Prop :->: Prop
  | Prop :<->: Prop

  deriving (Eq, Read)

-- Pentru Exercitiul 8. Extindeți tipul de date Prop și funcțiile definite până acum pentru a include conectorii logici
infixr 0 :<->:
infixr 1 :->:

infixr 2 :|:
infixr 3 :&:

-- 1a. (P Or Q) And (P And Q)
pa :: Prop
pa = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

-- 1b. (P Or Q) And (Not P And Not Q)
pb :: Prop
pb = (Var "P" :|: Var "Q") :&: ((Not (Var "P")) :&: (Not (Var "Q")))

-- 1c. (P And (Q Or R)) And ((Not P Or Not Q) And (Not P Or Not R))
pc :: Prop
pc = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

-- Observatie: E musai cu paranteza dupa Not (Exemplu: Not (Var "P"))

-- 2. Faceți tipul Prop instanță a clasei de tipuri Show, înlocuind conectorii Not, :|: și :&: cu ~, | și & și folosind direct numele variabilelor în loc de construcția Var nume

instance Show Prop where
    -- Var Nume
    show (Var nume) = nume
    -- | F
    show F = "F"
    -- | T
    show T = "T"
    -- | Not Prop
    show Not prop = "(~" ++ show prop ++ ")"
    -- | Prop :|: Prop
    show prop1 :|: prop2 = "(" ++ show prop1 ++ " | " ++ show prop2 ++ ")"
    -- | Prop :&: Prop
    show prop1 :&: prop2 = "(" ++ show prop1 ++ " & " ++ show prop2 ++ ")"

    -- Pentru Exercitiul 8. Extindeți tipul de date Prop și funcțiile definite până acum pentru a include conectorii logici
    show prop1 :->: prop2 = "(" ++ show prop1 ++ " -> " ++ show prop2 ++ ")"
    show prop1 :<->: prop2 = "(" ++ show prop1 ++ " <-> " ++ show prop2 ++ ")"
 
test_ShowProp :: Bool
test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

-- 3. Definiți o funcție eval care, dată fiind o expresie logică și un mediu de evaluare, calculează valoarea de adevăr a expresiei.

type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval (Var x) env = impureLookup x env
eval T _ = True
eval F _ = False
eval (Not p) env = not (eval p env)
eval (p1 :|: p2) env = eval p1 env || eval p2 env
eval (p1 :&: p2) env = eval p1 env && eval p2 env

-- Pentru Exercitiul 8. Extindeți tipul de date Prop și funcțiile definite până acum pentru a include conectorii logici
eval (p1 :->: p2) env = not (eval p1 env) || eval p2 env
eval (p1 :<->: p2) env = eval p1 env == eval p2 env
 
test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

-- 4. Definiți o funcție variabile care colectează lista tuturor variabilelor dintr-o formulă. Hint: folosiți funcția nub.

variabile :: Prop -> [Nume]
variabile p = nub (vars p) where
    vars (Var x) = [x]
    vars (Not p) = vars p
    vars (p1 :|: p2) = vars p1 ++ vars p2
    vars (p1 :&: p2) = vars p1 ++ vars p2
    vars T = []
    vars F = []

    -- Pentru Exercitiul 8. Extindeți tipul de date Prop și funcțiile definite până acum pentru a include conectorii logici
    vars (p1 :->: p2) = vars p1 ++ vars p2
    vars (p1 :<->: p2) = vars p1 ++ vars p2
 
test_variabile =
  variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

-- 5. Dată fiind o listă de nume, definiți toate atribuirile de valori de adevăr posibile pentru ea.

envs :: [Nume] -> [Env]
envs [] = [[]]
envs (x : xs) = [ (x, val) : rest | val <- [False, True], rest <- envs xs ]
 
test_envs = 
    envs ["P", "Q"]
    ==
    [ [ ("P",False)
      , ("Q",False)
      ]
    , [ ("P",False)
      , ("Q",True)
      ]
    , [ ("P",True)
      , ("Q",False)
      ]
    , [ ("P",True)
      , ("Q",True)
      ]
    ]

-- 6. Definiți o funcție satisfiabila care, dată fiind o propoziție, verifică dacă aceasta este satisfiabilă. Hint: puteți folosi rezultatele de la exercițiile 4 și 5

satisfiabila :: Prop -> Bool
satisfiabila p = any (eval p) (envs (variabile p))

-- Explicatie: Verifica daca exista vreun mediu in acea lista pentru care eval p mediu returneaza true
 
test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

-- 7. O propoziție este validă dacă se evaluează la True pentru orice interpretare a variabilelor. O formulare echivalentă este aceea că o propoziție este validă dacă negația ei este nesatisfiabilă. Definiți o funcție valida care verifică dacă o propoziție este validă.

valida :: Prop -> Bool
valida prop = not (satisfiabila (Not prop))

-- Explicatie: O propoziție este validă dacă negația ei este nesatisfiabilă

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True

-- 8. Extindeți tipul de date Prop și funcțiile definite până acum pentru a include conectorii logici -> (implicație) și <-> (echivalență), folosind constructorii :->: și :<->:

-- Vezi Exercitiile: 1, 2, 3, 4 fix dupa comentariul "Pentru Exercitiul 8"

-- 9. Două propoziții sunt echivalente dacă au mereu aceeași valoare de adevăr, indiferent de valorile variabilelor propoziționale. Scrieți o funcție care verifică dacă două propoziții sunt echivalente

echivalenta :: Prop -> Prop -> Bool
echivalenta p1 p2 = all (\env -> eval p1 env == eval p2 env) envs_comune where
    vars_comune = variabile (p1 :|: p2)
    envs_comune = envs vars_comune
 
test_echivalenta1 =
  True
  ==
  (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 =
  False
  ==
  (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 =
  True
  ==
  (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))
