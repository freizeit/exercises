module Main
where


{-
	A program solving

		http://code.google.com/codejam/contest/dashboard?c=351101#s=p0

	The test cases are contained in a file whose path is to be specified on
	the command line. The caller may also specify the number of CPU cores to
	use (default: 2).

	Example:

		./Main -f A-large-practice.in +RTS -N4 -RTS

	This will process the test cases contained in the 'A-large-practice.in'
	file and use three CPU cores.

	Please note: the results will only be ordered correctly for n=1. For
	n > 1 the results are output in random order.
-}


import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Parallel.Strategies
import Data.Either (lefts, rights)
import System.Environment
import qualified Input as I


main :: IO ()
main = do
    [fn] <- getArgs
    input <- I.parseFile fn
    case input of
        Nothing -> putStrLn "Malformed input file"
        Just ts ->
            -- mapM_ putStrLn $ (parMap rseq) handleTask wellformedTasks
            -- return (parMap rseq handleTask wellformedTasks) >> return ()
            print $ length ((parMap rseq) handleTask wellformedTasks)
            where
                wellformedTasks = zip [1..] (rights ts)


-- Make sure the task is calculated and print the results accordingly.
handleTask :: (Integer, I.Task) -> String
handleTask (i,t) =
    case calculate t of
        Nothing -> ("Case #" ++ (show i) ++ ": no solution found")
        Just (i1, i2) ->
            ("Case #" ++ (show i) ++ ": " ++ (show i1) ++ " " ++ (show i2))
        
    
-- Calculate a single task.
calculate :: I.Task -> Maybe (Integer, Integer)
calculate t =
    calculate' x xs c
    where
        -- start with the first item in the list ..
        x = (1, head (I.items t))
        -- .. and see whether adding any of the remaining items maxes out the
        -- store credit.
        xs = zip [2,3..] (tail (I.items t))
        c = I.credit t
        calculate' _ [] _ = Nothing -- failure case.
        calculate' (i1, x1) xs c =
            -- Look for the first element in xs where the sum matches c
            let l = dropWhile (\(_, x2) -> x1 + x2 /= c) xs in
            case l of
                [] -> calculate' (head xs) (tail xs) c
                -- success, sum of 2 items matches the store credit.
                ((i2,_):_) -> Just (i1, i2)
