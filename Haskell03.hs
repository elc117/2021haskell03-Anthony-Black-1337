--Pratica de Haskell parte 03 
--Nome : Anthony Carlos Da Silva

--Q1

add10toall :: [Int] -> [Int]
add10toall x =  [x+10| x <- x]

--Q2 

multN :: Int -> [Int] -> [Int]
multN  n l = [n*c| c <- l]

--Q3

multN' :: Int -> [Int] -> [Int]
multN' n1 l1 = map (\d -> n1 * d) l1

--Q4 

applyExpr :: [Int] -> [Int]
applyExpr b = [3*c+2| c <-b]

--Q5


applyExpr' :: [Int] -> [Int]
applyExpr' d = map (\c ->  3*c+2) d  

--Q6

addSuffix :: String -> [String] -> [String]
addSuffix s ls = [c++s| c<-ls]

--Q7

selectgt5 :: [Int] -> [Int]
selectgt5 f = [ c |c <- f, c > 5]

--Q8

sumOdds :: [Int] -> Int
sumOdds g = sum [ c|c <- g , mod c 2 /= 0]       

--Q9

sumOdds' :: [Int] -> Int
sumOdds' h = sum(filter ( \c -> mod c 2 /= 0 )h)

--Q10

selectExpr :: [Int] -> [Int]
selectExpr i = [c | c <-i, mod c 2 == 0 && c >= 20 && c <= 50]

--Q11

countShorts :: [String] -> Int
countShorts j = length [c  | c <- j, length c < 5]

--Q12
calcExpr :: [Float] -> [Float]
calcExpr k =  [ x^2/2  | x <- k, x^2/2 > 10] 

--Q13

trSpaces :: String -> String
trSpaces l = [if c /= ' ' then c else '-' | c <- l]

--Q14
selectSnd :: [(Int,Int)] -> [Int]
selectSnd m = [ snd c | c <- m]

--Q15

dotProd :: [Int] -> [Int] -> Int
dotProd n o = sum [z * y| (z,y)<- zip n o]

