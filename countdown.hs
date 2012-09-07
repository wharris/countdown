import Data.Maybe
import Data.List
import Game
import System.Environment ( getArgs )

-- nice games:
-- 25 50 75 100 3 6 = 952
-- 75 80 2 3 8 7 = 812

parseArgs :: [String] -> Game
parseArgs args 
    | length args == 7 = Game numbers target
    | otherwise        = error "usage: number*6 target"
    where numbers = map read (init args)
          target = read (last args)

main = do
    args <- getArgs
    putStrLn . show . bestAnswer . parseArgs $ args
