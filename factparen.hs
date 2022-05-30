import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe

genPrimes n= Foldable.toList(primes 2 4 (Seq.fromList ([False,False] ++ (replicate (n-1) True))))
    where primes x y l
            | x==n = l
            | y>n = primes (x+1) ((x+1)*(x+1)) l
            | not(Maybe.fromJust(l Seq.!? x)) = primes (x+1) ((x+1)*(x+1)) l
            | otherwise = primes x (y+x) (Seq.update y False l)
            
isItPrime = genPrimes 100000

num2paren :: (Integral i) => (i,i) -> [Char]
num2paren (x,y) = case(x,y) of
    (x,0) -> error "Division by 0"
    (0,y) -> ""
    (x,1) -> if x<0 then error "Number smaller than 0" 
                  else "(" ++ concat [num2paren (z,1) | z <- calculateList x isItPrime] ++ ")"
    (x,y) -> if gcd x y > 1 then num2paren(x `div` (gcd x y), y `div` (gcd x y))
                  else if (x<0) /= (y<0) then error "Number smaller than 0" 
                  else if (x<0) && (y<0) then num2paren(abs x, abs y) 
                  else num2paren(x,1) ++ concat [num2paren (z,1) | z <- calculateList y filteredisItPrime]
                  
    where calculateList a pri= (map abs . combineCommas . removeZeroes . addCommas . factorizeNumber pri) a
    
          factorizeNumber pri a = factorize pri a 1 [fromIntegral(0)]
              where factorize pri a p l
                      | a==1 = l
                      | not (pri!!p) = factorize pri a (p+1) l
                      | (pri!!p) && not(a `mod` (fromIntegral p)==0) = factorize pri a (p+1) (l ++ [0])
                      | (pri!!p) && (a `mod` (fromIntegral p)==0) = factorize pri (a `div` (fromIntegral p)) p ((init l) ++ [(last l)+1])
          
          addCommas a = (init . concat . List.transpose) [a, replicate (length a) (-1)]

          removeZeroes = filter (\a -> a/=0) 

          combineCommas a
            | length a <= 1 = a
            | otherwise = if (last a > 0) || (last(init a) > 0)
                then combineCommas(init a) ++ [last a]
                else combineCommas(init(init a) ++ [last(init a) + last a])
          
          filteredisItPrime = [False] ++ [if (x `mod` fromIntegral w ==0) then False else isItPrime!!w | w <- [1..length isItPrime]]

int2paren x = num2paren(x,1)

paren2num :: (Integral i) => [Char] -> (i,i)
paren2num x
  | (length . parenList . filterOut) x == 0 = (0,1)
  | (length . parenList . filterOut) x == 1 = (firstNum,1)
  | otherwise = (firstNum, secondNum)
    where firstNum = (string2num isItPrime . head . parenList . filterOut) x
          
          secondNum = (string2num filteredisItPrime . (\x->"(" ++ (concat . tail) x ++ ")") . parenList . filterOut) x
          
          filterOut a= filter (\x -> x=='(' || x==')') a
          
          parenList "" = []
          parenList a = if head a=='(' then recParenList (tail a) "(" [] 1
              else error "Invalid string"
          recParenList a s l n
            | n<0 = error "Invalid string"
            | a=="" && n/=0 = error "Invalid string"
            | a=="" && n==0 = l ++ [s]
            | n==0 && head a == ')' = error "Invalid string"
            | n==0 = recParenList (tail a) "(" (l ++ [s]) 1
            | otherwise = recParenList (tail a) (s ++ [head a]) l (n + (if (head a) == '(' then 1 else (-1)))
          
          string2num pri s = (calcNum pri . map (string2num isItPrime) . parenList . tail . init) s
          
          calcNum pri l = calcNumRec pri l 1 0 ((length l) `mod` 2 == 0)
          calcNumRec pri l p n c
            | length l == 0 = p
            | not (pri!!n) = calcNumRec pri l p (n+1) c
            | (pri!!n) && not c = calcNumRec pri (tail l) (p*(fromIntegral(n)^(head l))) n (not c)
            | (pri!!n) && c && (head l==0) = calcNumRec pri (tail l) p n (not c)
            | (pri!!n) && c && (head l>0) =  calcNumRec pri ([head l - 1] ++ (tail l)) p (n+1) c
            
          filteredisItPrime = [False] ++ [if (firstNum `mod` fromIntegral w ==0) then False else isItPrime!!w | w <- [1..length isItPrime]]

paren2int x
  | snd (paren2num x) == 1 = fst (paren2num x)
  | otherwise = error "Not an integer"

paren2float :: [Char] -> Double
paren2float x = (fromIntegral . fst . paren2num) x  / (fromIntegral . snd . paren2num) x
