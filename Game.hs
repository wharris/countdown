module Game
    (Game(Game)
    ,bestAnswer
    ,bestAnswers
    )
where
import Data.Maybe
import Data.List
import Expression

data Game = Game [Int] Int

without :: (Eq a) => (a, a) -> [a] -> [a]
without (i1, i2) xs = (delete i1) . (delete i2) $ xs

expressions :: (Expression -> Expression -> Expression) -> [Expression]
                -> [[Expression]]
expressions func xs =
    filter (isJust . eval . head) [func i1 i2 : without (i1, i2) xs |
                                   (i1, i2) <- combination xs]

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


join sep = foldl1 (\x y -> x ++ sep ++ y)

play :: Game -> [[Expression]]
play (Game xs _) = solutions $ map Equ xs

target :: Game -> Int
target (Game _ target) = target

bestAnswers :: Game -> [Expression]
bestAnswers g = best (target g) Nothing (play g)

bestAnswer = last . bestAnswers

