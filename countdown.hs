import Data.Maybe
import Data.List

data Expression =
    Add Expression Expression |
    Sub Expression Expression |
    Mul Expression Expression |
    Div Expression Expression |
    Equ Int |
    Nowt
    deriving (Show, Eq)

eval' :: (a -> b -> c) -> (Maybe a) -> (Maybe b) -> (Maybe c)
eval' f Nothing _ = Nothing
eval' f _ Nothing = Nothing
eval' f x y = Just (f (fromJust x) (fromJust y))

maybeDiv :: Maybe Int -> Maybe Int -> Maybe Int
maybeDiv x y
    | y == (Just 0)             = Nothing
    | eval' mod x y == Just 0   = eval' div x y
    | otherwise                 = Nothing

eval :: Expression -> Maybe Int
eval Nowt = Nothing
eval (Equ x)   = Just x
eval (Add x y) = eval' (+) (eval x) (eval y)
eval (Sub x y) = eval' (-) (eval x) (eval y)
eval (Mul x y) = eval' (*) (eval x) (eval y)
eval (Div x y) = maybeDiv (eval x) (eval y)

repr :: Expression -> String
repr Nowt = "Nowt"

repr (Equ x) = show x


repr (Add x y) = (repr x) ++ "+" ++ (repr y)

repr (x `Sub` (Equ y)) = (repr x) ++ "-" ++ (show y)
repr (x `Sub` (Mul y z)) = (repr x) ++ "-" ++ (repr (Mul y z))
repr (x `Sub` (Div y z)) = (repr x) ++ "-" ++ (repr (Div y z))
repr (x `Sub` y) = (repr x) ++ "-(" ++ (repr y) ++ ")"

repr ((Equ x) `Mul` (Equ y)) = (show x) ++ "*" ++ (show y)
repr ((Mul x y) `Mul` (Mul i j)) = (repr (Mul x y)) ++ "*" ++ (repr (Mul i j))
repr ((Mul x y) `Mul` (Div i j)) = (repr (Mul x y)) ++ "*" ++ (repr (Div i j))
repr (x `Mul` y) = "(" ++ (repr x) ++ ")*(" ++ (repr y) ++ ")"

repr ((Equ x) `Div` (Equ y)) = (show x) ++ "/" ++ (show y)
repr ((Mul x y) `Div` (Equ z)) = (repr (Mul x y)) ++ "/" ++ (show z)
repr (x `Div` (Equ y)) = "(" ++ (repr x) ++ ")/" ++ (show y)
repr (x `Div` y) = "(" ++ (repr x) ++ ")/(" ++ (repr y) ++ ")"


one = Equ 1
two = Equ 2
three = Equ 3
oneAddOne = Add one one

equation = three `Add` (two `Mul` one)

showOrNothing Nothing = "Nothing"
showOrNothing x = show . fromJust $ x

showEquation x = (repr x) ++ " = " ++ (showOrNothing (eval x))


without :: (Eq a) => (a, a) -> [a] -> [a]
without (i1, i2) xs = (delete i1) . (delete i2) $ xs

expressions :: (Expression -> Expression -> Expression) -> [Expression] -> [[Expression]]
expressions func xs =
    filter (isJust . eval . head) [func i1 i2 : without (i1, i2) xs | (i1, i2) <- combination xs]

allExpressions :: [Expression] -> [[Expression]]
allExpressions [] = []
allExpressions xs =
    expressions Add xs ++
    expressions Sub xs ++
    expressions Mul xs ++
    expressions Div xs

solutions :: [Expression] -> [[Expression]]
solutions [] = []
solutions xs =
    let all = allExpressions xs
    in  all ++ foldl (++) [] (map solutions all)

combination [] = []
combination (x:xs) = [(x, x') | x' <- xs] ++ combination xs


game = map Equ [2,2,1,75,100,10]


join sep = foldl1 (\x y -> x ++ sep ++ y)

showGame es = join ", " $ map showEquation es

listGames [] = putStr ""
listGames (g:gs) = putStrLn (showGame g) >> listGames gs

main = listGames (solutions game)
