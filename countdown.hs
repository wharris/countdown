import Data.Maybe
import Data.List
import Game

game = Game [25,50,75,100,3,6] 952
game2 = Game [75, 50, 2, 3, 8, 7] 812

main = putStrLn . show . bestAnswer $ game
