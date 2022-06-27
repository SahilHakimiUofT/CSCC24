module FixMe where
import Debug.Trace

{-
pow b e computes the exponentiation: b raised to the power e (written b^e
below).  Don't worry about negative e, we will assume e>=0 (make it our
precondition).

Algorithm explanation:

e could be even or odd, i.e., let
  q = e div 2
  r = e mod 2
then e = 2q+r, and r could be 0 or 1.

(In Haskell this can be done with one single "(q, r) = divMod e 2" so the bugs
are elsewhere!)

* If r=0:

    b^e = b^(2q) = (b^q)^2

  so use a recursive call to get b^q, then just square its answer.

* If r=1:

    b^e = b^(2q+1) = b^(2q) * b = (b^q)^2 * b

  so use a recursive call to get b^q, then square its answer, then multiply an
  extra b.

And now we need a base case: b^0 = 1.

My code below contains syntax errors, typos, and bugs.  Fix my code!  But please
don't fix what's right.
-}

pow :: Integer -> Integer -> Integer
pow b e
    | e == 0 = 1
    | r == 0 = y2
    | r == 1 = y2*b
 
  where
    (q, r) = divMod e 2
    y = pow b q
    y2 = y * y


rev :: [Integer] -> [Integer]
rev xs = revHelper xs []

revHelper :: [Integer] -> [Integer] -> [Integer]
revHelper [] acc = acc
revHelper (x:xs) acc = revHelper xs (x:acc)

appender :: [Integer] -> [Integer] -> [Integer]
appender [] y = y
appender (xs:xt) y = xs:appender xt y

findLastElement::[a] -> a
findLastElement (x:[]) = x
findLastElement (x:xt) = findLastElement xt


findSecondLastElement::Eq a => [a] -> a
findSecondLastElement (x:xt:xs:xl)
  |xl == [] = xt
  |otherwise = findSecondLastElement (xt:xs:xl)
findSecondLastElement (x:xt:xs)
  |xs == [] = x
  |otherwise = findSecondLastElement(xt:xs)


myLength :: [a] -> Integer
myLength x = myLengthHelper x 0 


myLengthHelper :: [a] -> Integer -> Integer
myLengthHelper [] x = x
myLengthHelper (x:xt) z = myLengthHelper xt z+1


atFourPlusAtSeven :: (Integer -> Integer) -> Integer
atFourPlusAtSeven f = f 4 + f 7


insertionSort :: (a -> a -> Bool) -> [a] -> [a]
insertionSort _ [] = []
insertionSort leq (x:xt) = insert x (insertionSort leq xt)
  where
    -- insert e xs = assuming xs is already sorted, insert e at "the right place"
    insert e [] = [e]
    insert e xs@(x:xt)
        | leq e x = e : xs
        | otherwise = x : insert e xt


data Tetrastratan
  = Monarch
  | Lord String String Integer
  | Knight String
  | Peasant String
  deriving (Eq, Show)

mkDuke :: String -> Integer -> Tetrastratan
mkDuke terr n = Lord "Duke" terr n


addressTetra :: Tetrastratan -> String
addressTetra Monarch = "H.M. The Monarch"
addressTetra (Lord d t i) = "The " ++ show i ++ "th " ++ d ++ " of " ++ t
addressTetra (Knight n) = "Sir/Dame " ++ n
addressTetra (Peasant n) = n


data IntegerBST = IEmpty | INode IntegerBST Integer IntegerBST
    deriving Show


ibstInsert :: Integer -> IntegerBST -> IntegerBST
ibstInsert x IEmpty = INode IEmpty x IEmpty
ibstInsert x (INode y z q)
  | x<z = INode (ibstInsert x y) z q
  | otherwise = INode y z (ibstInsert x q)


data BST a = Empty | Node (BST a) a (BST a)
  deriving Show


bstInsert :: Ord a => a -> BST a -> BST a
bstInsert x Empty = Node Empty x Empty
bstInsert x (Node y z q)
  | x<z = Node (bstInsert x y) z q
  | otherwise = Node y z (bstInsert x q)