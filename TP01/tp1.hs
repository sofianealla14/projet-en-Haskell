f :: Int -> Int    
f x = x + 2  

g :: Int -> Int 
g x = x - 5

-- 1. LES CARRES

square :: Int -> Int
square x = x*x

sumSquares :: Int -> Int -> Int
sumSquares x y = square x + square y

-- pour sumSquaresMax -> penser à utiliser la fonction (min x y)

sumSquaresMax :: Int -> Int -> Int -> Int
sumSquaresMax x y z = if x<y && x<z then sumSquares y z else (if y<x && y<z then sumSquares x z else sumSquares x y)

-- sumSquaresMax x y z
--   | min x y == x && min x z == x = sumSquares y z
--   | min x y == y && min y z == y = sumSquares x z
--   | otherwise                    = sumSquares x y


-- 2. CONVERTION : CELSIUS <--> FAHRENHEINT

c2f :: Float -> Float
c2f c = 9 / 5 * c + 32

f2c :: Float -> Float
f2c f = 5 / 9 * ( f - 32 )


-- 3. REMBOURSEMENT selon tarifs kilometriques 

-- trame à compléter

reductionRate :: Float -> Float
reductionRate nbPers 
   | nbPers < 2  = 0.52
   | nbPers < 5  = 0.52*0.75
   | nbPers < 11 = 0.52*0.50
   | otherwise   = 0.52*0.25

travelExpenses :: Float -> Float -> Float
travelExpenses km nbPers = nbPers * km * reductionRate nbPers


-- 4. NEXT UPPER CASE

decode :: Int -> Char
decode n = toEnum n

code :: Char -> Int
code c = fromEnum c

nextUpperCase :: Char -> Char
nextUpperCase char = if char == 'Z' then decode((code char)-25) else decode((code char)+1)


-- 5. COLLATZ FUNCTION

-- 2 trames à compléter

collatz :: Int -> Int
collatz n
  | n == 2 = 1
  | odd n  = 3 * n +1
  | even n = div n 2
  
nbCalls :: Int -> Int
nbCalls n
 | n == 1 = 0
 | otherwise = 1 + nbCalls (collatz n)

syracuse :: Int -> [Int]
syracuse n
  | n == 2    = [2]
  | otherwise = n : syracuse (collatz n)

-- une autre définition possible de nbCalls en utilisant syracuse

nbCallsBis :: Int -> Int
nbCallsBis n = length (syracuse n)

-- Si la conjecture de Collatz était fausse, on suppose qu'il y aurait une boucl infini


-- 6. TOUS PAIRS

-- trame à compléter OU définir avec des if imbriqués OU en utilisant des gardes

allEven :: [Int] -> Bool
allEven [] = True
allEven (x:xs)
  | odd x  = False
  | even x = allEven (tail (x:xs))


-- 7. RIRE

-- ci dessous un ecriture en utilisant le pattern-matching sur les Int

laugh :: Int -> [Char]
laugh 0 = ""
laugh n = "HA " ++ laugh (n-1)

-- TODO définir laugh à l'aide d'un if ou bien en utilisant des gardes

-- laugh n
--  | n == 0    = ""
--  | otherwise = "HA " ++ laugh (n-1)


-- 8. DOUBLER : cas general et cas particulier

-- Le cas general
doubleG :: [a] -> [a]
doubleG []     = []
doubleG (x:xs) = x:(x:(doubleG xs))

-- Mais, pour l'exo, il faut "aplatir" les String par concatenation

double :: [String] -> String
double (x:xs) = head(x:xs) ++ double(xs)


-- 9. SOMME DES CARRES DES N PREMIERS NOMBRE PAIRS

{- sumSquareEven :: Int -> Int
sumSquareEven n
  | n == 0 = 0
  | otherwise = sumSquareEven(n-1) + -- carre de nombre pair -}


-- 10. FONCTION MYSTERE

-- Tester mystery sur des listes d'entiers et conclure

mystery :: [Int] -> [Int]
mystery []     = []
mystery (x:xs) = mystery [y | y <- xs, y <= x] ++  [x] ++ mystery [y | y <- xs, y > x]

-- Mystery 

