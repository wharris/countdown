import Data.Maybe
import Data.List
import Expression
import Game

solved :: Int -> (Maybe Expression) -> Bool
solved _ Nothing = False
solved target (Just closest)
    | isNothing (eval closest) = False
    | otherwise = (==) target (fromJust . eval $ closest)

distance :: (Num a) => a -> a -> a
distance x y = abs (x - y)

better :: Int -> (Maybe Int) -> (Maybe Int) -> Bool
better _ Nothing _ = False
better _ (Just x) Nothing = True
better target x y = distance target (fromJust x) < distance target (fromJust y)

betterExpression :: Int -> Expression -> (Maybe Expression) -> Bool
betterExpression _ _ Nothing = True
betterExpression target x y = better target (eval x) (eval . fromJust $ y)

best :: Int -> (Maybe Expression) -> [[Expression]] -> [Expression]
best _ _ [] = []
best target closest (g:gs)
    | solved target closest = []
    | betterExpression target (head g) closest = head g : best target (Just . head $ g) gs
    | otherwise = best target closest gs

bestAnswers :: Game -> [Expression]
bestAnswers g = best (target g) Nothing (play g)

printLines :: [String] -> IO ()
printLines [] = putStr ""
printLines (x:xs) = putStrLn x >> printLines xs

listExpressions :: [Expression] -> IO ()
listExpressions = printLines . map showEquation

game = Game [25,50,75,100,3,6] 952
game2 = Game [75, 50, 2, 3, 8, 7] 812

main = listExpressions $ bestAnswers game2
