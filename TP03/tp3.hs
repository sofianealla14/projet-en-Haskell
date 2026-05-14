
-- TP3 : Arbres Binaires de Recherche (ABR)

data Btree a = Nil
             | Bin a (Btree a) (Btree a)
     deriving (Show, Ord, Eq)

-- Les 3 ABR donnés comme exemples dans l'énoncé 

a1 = (Bin 4 (Bin 3 (Bin 2 Nil Nil) Nil) (Bin 7 (Bin 6 Nil Nil) (Bin 8 Nil Nil)))

a2 = (Bin 4 (Bin 3 (Bin 1 Nil Nil) Nil) (Bin 8 (Bin 7 Nil Nil) (Bin 11 (Bin 9 Nil Nil) Nil)))

a3 = (Bin 40 (Bin 30 (Bin 20 (Bin 15 Nil Nil) (Bin 25 Nil Nil)) (Bin 35 Nil Nil)) (Bin 70 (Bin 60 (Bin 50 (Bin 45 Nil Nil) Nil) (Bin 65 Nil Nil)) (Bin 80 Nil Nil)))

-- Petit outil de visualisation (pour mieux comprendre)

voir :: (Show a) => (Btree a) -> IO()
voir t = putStr (visuTree t)

{- 	voir a1
	voir a2
	voir a3 -}

visuTree :: Show a => Btree a -> String
visuTree t = visu t 1
   where visu Nil n = ""
         visu (Bin y t1 t2) n = (visu t1 (n+5)) ++ 
                                [' ' | i <- [1..n]] ++ (show y) ++ ['\n'] ++
                                (visu t2 (n+5))


-- 1. Premiers Pas
a4 = (Bin 10 a1 a3) 

a5 = (Bin 17 a1 a3)

{-	voir a4 
	voir a5 (a5 n'est pas un ABR car 15 plus petit que 17 alors qu'il est dans l'arbre gauche de 17 -> il est censé être -}

profondeur :: Btree a -> Int
profondeur Nil = 0
profondeur (Bin a Nil Nil) = 0
profondeur (Bin a t1 t2) = 1 + max (profondeur t1) (profondeur t2)

{- 	profondeur (Bin 2 Nil Nil) ==> 0 
	profondeur a1 ==> 2  -}

prefixe :: Btree a -> [a]
prefixe Nil = []
prefixe (Bin a t1 t2) = a:(prefixe t1)++(prefixe t2)

infixe :: Btree a -> [a]
infixe Nil = []
infixe (Bin a t1 t2) = (infixe t1)++(a:(infixe t2))

postfixe :: Btree a -> [a]
postfixe Nil = []
postfixe (Bin a t1 t2) = (postfixe t1)++(postfixe t2)++(a:[])

{-	prefixe a1 ==> [4,3,2,7,6,8]
	infixe a1 ==> [2,3,4,6,7,8]
	postfixe a1 ==> [2,3,6,8,7,4] -}


-- 2. Appartenance d'un élément a un ABR
inBtree :: Ord a => a -> Btree a -> Bool
inBtree x Nil = False
inBtree x (Bin y t1 t2)
         | x < y     = inBtree x t1
         | x > y     = inBtree x t2
         | otherwise = True

{- 	inBtree 2 a1 ==> True
	inBtree 20 a1 ==> False -}


-- 3. Insérer un élément dans un ABR
insere :: Ord a => a -> Btree a -> Btree a
insere x Nil = (Bin x Nil Nil)
insere x (Bin y t1 t2)
   | x < y = Bin y (insere x t1) t2
   | x > y = Bin y t1 (insere x t2)
   | otherwise = Bin y t1 t2

list2abr :: Ord a => [a] -> (Btree a)
list2abr [] = Nil
list2abr (x:xs) = insere x (list2abr xs)

trier :: Ord a => [a] -> [a]
trier xs = infixe (list2abr xs)

path :: Ord a => a -> Btree a -> [a]
path x Nil = []
path x (Bin y t1 t2)
   | x < y = y:(path x t1)
   | x > y = y:(path x t2)
   | otherwise = y:[]

{-	path a3 15 ==> [40, 30, 20, 15]
	path a3 20 ==> [40, 30, 20]
	path a3 30 ==> [40, 30]
	path a3 40 ==> [40] -}


-- 4. Un ABR est-il équilibré ?
-- Constater qu'il y a des cas pathologiques (voir a6)
a6 = list2abr [1..10]

-- Version naive -- nombre pathologique d'appels à la fonction profondeur

estEquilibre :: Btree a -> Bool
estEquilibre Nil = True
estEquilibre (Bin _ t1 t2) = (abs (profondeur t1 - profondeur t2) <= 1)
                             && estEquilibre t1
                             && estEquilibre t2

-- Cette définition engendre un nombre d’appels ”excessivement” élevé à la fonction (profondeur t) car 


-- 5. ABR portant des entiers : Suppression

sommeArbre :: Btree Integer -> Integer
sommeArbre Nil = 0
sommeArbre (Bin y t1 t2) = y + sommeArbre t1 + sommeArbre t2

{-	sommeArbre a1 ==> 30
	sommeArbre a2 ==> 43
	sommeArbre a3 ==> 535 -}

minArbre :: Btree Integer -> Integer
minArbre (Bin y t1 t2) = min y (min (minArbre t1) (minArbre t2))

maxArbre :: Btree Integer -> Integer
maxArbre (Bin y t1 t2) = min y (min (maxArbre t1) (maxArbre t2))

{-	minArbre a4 ==> 2
	maxArbre a4 ==> 80 -}

join :: Btree Integer -> Btree Integer -> Btree Integer
join t1 Nil = t1
join Nil t2 = t2
join (Bin x u1 u2) t2 = Bin x u1 (join u2 t2)

{- delete :: Integer -> Btree Integer -> Btree Integer
delete x Nil =
delete x (Bin y t1 t2)
  |x < y = 
  |x > y = 
  |otherwise =  -}

{- INDICATION pour définir la fonction delete1 
on pourra se servir de la fonction suivante :

join1 :: Btree Integer -> Btree Integer -> Btree Integer
join1 t1 Nil = t1
join1 Nil t2 = t2
join1 t1 t2  = Bin (minArbre t2) t1 (deleteMin t2)
-- ou bien join1 t1 t2 = Bin (maxArbre t1) (deleteMax t1) t2

-}


-- 6. Version efficace 
-- La profondeur d’un sous-arbre n’est calculée qu’une seule fois.

estEquilibreBis:: Btree a -> Bool
estEquilibreBis t = fst (aux t)

aux :: (Btree a) -> (Bool, Int)
aux Nil = (True, -1)
aux (Bin _ fg fd)
   | not eqg || not eqd || abs (htg - htd) > 1 = (False, 0)
   | otherwise                                 = (True, 1 + (max htg htd))
   where (eqg, htg) = aux fg
         (eqd, htd) = aux fd 

