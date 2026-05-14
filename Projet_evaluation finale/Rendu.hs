type TilePos = (Int, Int)

type State = [TilePos]

-- Exemples d'etats pour tous les tests
e, s, ef, a, x0, x1, x2, x3 :: State

-- Etat final
ef = [(2,2),(1,1),(2,1),(3,1),(3,2),(3,3),(2,3),(1,3),(1,2)]

-- l'exemple de l'enonce de la Partie 1 du DM
e = [(2,2),(1,1),(2,1),(3,1),(2,3),(3,2),(3,3),(1,3),(1,2)]

-- l'exemple etudie lors du CM
s = [(2,3),(1,2),(1,1),(3,1),(3,2),(3,3),(2,2),(1,3),(2,1)]

-- les exemples pour tester les parties 2 et 3
x0 = [(2,3),(1,1),(3,2),(2,1),(3,1),(3,3),(2,2),(1,3),(1,2)]
x1 = [(1,2), (3,1),(1,1),(3,2),(2,2),(3,3),(2,3),(1,3),(2,1)]
x2 = [(1,3),(3,1),(1,1),(3,2),(1,2),(3,3),(2,2),(2,3),(2,1)]
x3 = [(2,2),(3,3),(2,3),(1,3),(1,2),(1,1),(2,1),(3,1),(3,2)]

{- PARTIE 1 -}
-- 1.1 Representation d'un etat
whichTileAt :: TilePos -> State -> Int
whichTileAt pos s
   | fst (head s) == fst pos && snd (head s) == snd pos = 0
   | otherwise = 1 + whichTileAt pos (tail s)

posTile :: Int -> State -> TilePos
posTile i s
   | i == 0 = head s
   | otherwise = posTile (i-1) (tail s)

posEmpty :: State -> TilePos
posEmpty s = posTile 0 s

-- 1.2 Representation d'un etat
toString :: State -> String
toString s = row 1 s ++ row 2 s ++ row 3 s ++ "\n"

row :: Int -> State -> String
row n s = " " ++ t 1 ++ " " ++ t 2 ++ " " ++ t 3 ++ "\n"
   where t m = show (whichTileAt (m, n) s)

toStr :: [State] -> String
toStr [] = ""
toStr (s:ss) = toString s ++ toStr ss

toStr2 :: [State] -> String
toStr2 [] = ""
toStr2 (s:ss) = concat(map toString (s:ss))

-- 1.3 Distances entre cases
distHamming :: TilePos -> TilePos -> Int
distHamming pos1 pos2
   | fst pos1 == fst pos2 && snd pos1 == snd pos2 = 0
   | otherwise = 1

distManhattan :: TilePos -> TilePos -> Int
distManhattan pos1 pos2 = (abs ((fst pos1) - (fst pos2))) + (abs((snd pos1) - (snd pos2)))

-- 1.4 Fonctions heuristiques h1 et h2
h1 :: State -> Int
h1 [] = 0
h1 s = sum[distManhattan (s!!x) (ef!!x) | x<-[1..8]]

h2 :: State -> Int
h2 [] = 0
h2 s = sum[distHamming (s!!x) (ef!!x) | x<-[1..8]]

-- 1.5 ALTERNATIVE pour la fonction successeurs
-- SITUATION A : completer 
successeurs :: State -> [State]
successeurs s = [swap i s | i <- [1.. (length s) - 1], valide i s]
   where 
       valide i s = distManhattan (s!!0) (s!!i) == 1
       swap i s = [(s!!i)] ++ [(s!!j) | j<-[1..(i-1)]] ++ [(s!!0)] ++ [(s!!k) | k<-[i+1..(length s - 1)]]

-- Pour tester votre definition de successeurs
test :: State -> Bool
test s = (successeurs s) == (successeursBis s)

--SITUATION B :

successeursBis :: State -> [State]
successeursBis (e:ts) = inter [e] ts
       where inter (e:ts) [] = []
             inter (e:ts1) (t:ts2)
               | distManhattan e t == 1 = (t:ts1 ++ e:ts2) : (inter (e:ts1++[t]) ts2)
               | otherwise = inter (e:ts1++[t]) ts2
               

{- PARTIE 2 -}

-- 2.1 Préliminaires

-- 2.2 Etude du parcours en largeur (Breadth First Search)
bfs :: State -> [State]
bfs s = bfsSolv [s] []

bfsSolv :: [State] -> [State] -> [State]
bfsSolv (s : ss) visited
   | s == ef = reverse (s : visited)
   | otherwise = bfsSolv (add_Bfs (remAlreadyVisited (successeurs s) visited) ss) (s : visited)
      where
         remAlreadyVisited xs ys = [x | x <- xs, not (elem x ys)]
         add_Bfs xs ys = ys ++ xs

-- 2.3 Parcours en profondeur (Depth First Search)
a = [(3,2),(1,1),(2,2),(2,1),(3,1),(3,3),(2,3),(1,3),(1,2)]

dfs :: State -> [State]
dfs s = dfsSolv [s] []

dfsSolv :: [State] -> [State] -> [State]
dfsSolv (s : ss) visited
   | s == ef = reverse (s : visited)
   | otherwise = dfsSolv (add_Dfs (remAlreadyVisited (successeurs s) visited) ss) (s : visited)
      where
         remAlreadyVisited xs ys = [x | x <- xs, not (elem x ys)]
         add_Dfs xs ys = xs ++ ys

-- 2.4 Parcours en meilleur d’abord (Best First Search)
-- pour définir la fonction heuristique h, choisir l’une des 2 définitions ci-dessous
-- h = h1
h = h2

bestfs :: State -> [State]
bestfs s = bestfsSolv [s] []
bestfsSolv :: [State] -> [State] -> [State]
bestfsSolv (s:ss) visited 
   | s == ef = reverse (s:visited)
   | otherwise = bestfsSolv (add_Bestfs (remAlreadyVisited (successeurs s) visited) ss) (s:visited)
      where 
         remAlreadyVisited xs ys = [x | x <-xs, not (elem x ys)]
         add_Bestfs [] ys = ys
         add_Bestfs (x : xs) ys = insert x (add_Bestfs xs ys)

insert :: State -> [State] -> [State]
insert s [] = [s]
insert s xs
 | h s < h (head xs) = [s]++xs
 | otherwise = [head xs] ++ insert s (tail xs)

{- PARTIE 3 -}

-- 3.1 Objectif de la partie 3 : definir la fonction (bfsPath s) voir 3.3

-- 3.2 Retrouver le chemin allant d’un état initial e0 a l’etat final ef
-- a) Completer parent
parent :: State -> [(State,State)] -> State
parent s [] = nil
parent s ((x, p) : cs)
   | (s == x) = p
   | otherwise = parent s cs

-- b) Completer nil
nil :: State
nil = []
findSolnPath :: State -> [(State,State)] -> [State]
findSolnPath s cs
   | parent s cs == nil = [s]
   | otherwise =  findSolnPath (parent s cs) cs ++ [s]

-- 3.3 Determiner le chemin allant d’un état initial s a l’etat final ef en utilisant un parcours en largeur
-- a) Commenter bfsPath
bfsPath :: State -> [State]
bfsPath s = bfsSolv2 [(s, nil)] []

-- b) Expliquer et completer bfsSolv2
bfsSolv2 :: [(State,State)] -> [(State,State)] -> [State]
bfsSolv2 [] _ = []
bfsSolv2 ((s,p):os) cs
   | s == ef = findSolnPath s (cs++[( s,p)])
   | otherwise = bfsSolv2 (add_Bfs2 (remAlreadyVisited [(x,s) | x <- successeurs s] cs) os) ((s,p) : cs)
      where remAlreadyVisited xs ys = [ (x,a) | (x,a) <- xs, not (elem x [ y | (y,b) <- ys])]
            add_Bfs2 xs ys = ys ++ xs
