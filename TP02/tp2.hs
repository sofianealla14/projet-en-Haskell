
type Facteur = Int
type Exposant = Int
type Couple = (Facteur, Exposant)
type Decomposition = [Couple]

-- Partie vue en CM 03

pfactors :: Int -> [Int]
pfactors n = pfactors' n primes
   where pfactors' x (p:ps)
            | p > x          = []
            | mod x p == 0   = p : (pfactors' (div x p) (p:ps))
            | otherwise      = pfactors' x ps

-- juste pour info, voici le type de pfactors' :: Int -> [Int] -> [Int]

prep :: [Int] -> Decomposition
prep [] = []
prep (x:xs) = (x, (length (takeWhile (==x) (x:xs))) ) : (prep (dropWhile (==x) xs))

int2rep :: Int -> Decomposition
int2rep = prep . pfactors


-- 1. Retourne l’entier associe à une décomposition
rep2int {-, evalBis-} :: Decomposition -> Int
rep2int(x:xs)
   | xs == [] = ((fst x) ^ (snd x))
   | otherwise = ((fst x) ^ (snd x)) * rep2int xs
   
{- rep2intBis [] = []
rep2intBis ((x,n):xs) = [ x ^ n | (x, n) <- xs] -}

{- 	rep2int [] ==> 1
	rep2int [(2,3), (5,2)] ==> 200
	rep2int [(2,1), (5,1), (7,3)] ==> 3430 -}


-- 2. Une décomposition représente un nombre premier ?
estPremier :: Decomposition -> Bool
estPremier xs
   | length xs /= 1 = False
   | snd (head xs) /= 1 = False
   | otherwise = True

{- 	estPremier [(2,3), (5,2)] ==> False
	estPremier [(2,1)] ==> True -}


-- 3. Calcule le PGCD de 2 entiers représentés par leur décomposition

pgcdBis :: Decomposition -> Decomposition -> Decomposition
{- pgcd, -} 

{- pgcd xs ys = [] -}

pgcdBis [] dec = []
pgcdBis dec [] = []
pgcdBis ((k1,d1):p1) ((k2,d2):p2)
   | k1 == k2 = (k1,min d1 d2) : ( pgcdBis p1 p2 )
   | k1 < k2 = ( pgcdBis p1 ([(k2,d2)]++p2) )
   | otherwise = ( pgcdBis ([(k1,d1)]++p1) p2 )
   
{-	pgcd [(2,3), (5,2)] [(2,1), (5,1), (7,3)] ==> [(2,1), (5,1)]
	pgcd [(2,3), (5,2)] [(2,2)] ==> [(2,2)]
	pgcd [(2,3), (5,2)] [(7,3)] ==> [] -}


-- 4. Calcule le PPCM de 2 entiers représentés par leur décomposition
-- (version récursive exactement sur le meme modèle que PGCD)
ppcm :: Decomposition -> Decomposition -> Decomposition
ppcm [] dec = dec
ppcm dec [] = dec
ppcm ((k1,d1):p1) ((k2,d2):p2)
   | k1 == k2 = (k1,max d1 d2) : ( ppcm p1 p2 )
   | k1 < k2 = (k1,d1) : ( ppcm p1 ([(k2,d2)]++p2) )
   | otherwise = (k2,d2) : ( ppcm ([(k1,d1)]++p1) p2 )

{- 	ppcm [(2,3), (5,2)] [(7,3)] ==> [(2,3),(5,2),(7,3)]
	ppcm [(2,3), (5,2)] [(2,2)] ==> [(2,3),(5,2)]
	ppcm [(2,3), (5,2)] [(2,1), (5,1), (7,3)] ==> [(2,3),(5,2),(7,3)] -}


-- 5. Determine le nombre de diviseurs du nombre entier dont la décomposition est xs
nbDiviseurs :: Decomposition -> Int
nbDiviseurs [] = 1
nbDiviseurs xs = (snd (head xs) + 1) * nbDiviseurs (tail xs)
  
{- nbDiviseurs [(2,1)] ==> 2
nbDiviseurs [(3,2)] ==> 3
nbDiviseurs [(2,1), (3,2)] ==> 6
nbDiviseurs [(2,1), (3,2), (7,3)] ==> 24 -}


-- 6. Retrouver la liste des diviseurs d’un entier à partir de sa décomposition
op :: Couple -> [Int] -> [Int]
op (x,n) ys = [(x^i) * y | i <- [0..n], y <- ys]
-- La fonction (op n ys) ci-dessus permet de 

diviseurs :: Decomposition -> [Int]
diviseurs [] = [1]
diviseurs xs = op (head xs) (diviseurs (tail xs))

{- 	diviseurs [(5,2)] ==> [1,5,25]
	diviseurs [(2,3), (5,2)] ==> [1,5,25,2,10,50,4,20,100,8,40,200]
	op (2,3) [1,5,25] ==> [1,5,25, 2,10,50, 4,20,100, 8,40,200] -}


-- 7. Premier exemple de traitement d’une liste infinie
primes :: [Int]
primes =  sieve [2 .. ]
       where sieve (p:xs) = p : (sieve [x | x <- xs, mod x p > 0])
       
{- 	primes ==> renvoie tous les nombre premiers jusqu'à l'infini (la fonction ne s'arrete jamais)
	take 10 primes ==> [2,3,5,7,11,13,17,19,23,29]
	takeWhile (<= 100) primes ==> [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
	length (takeWhile (<= 10000) primes) ==> 1229 -}
