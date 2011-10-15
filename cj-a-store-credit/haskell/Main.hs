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

import Control.Monad
import Data.Either (lefts, rights)
import System.Environment
import qualified Input as I

main = do
    args <- getArgs
    input <- I.parseFile $ head args
    case input of
        Nothing -> error "Malformed input file"
        Just ts -> do
            {-
            when (not (null malformedTasks)) (do
                putStrLn "Malformed task(s):"
                mapM_ (putStrLn . show) malformedTasks)
            when (not (null wellformedTasks)) (do
                putStrLn "Well-formed task(s):"
                mapM_ (putStrLn . show) wellformedTasks)
            -}
            mapM_ handleTask (zip [1..] wellformedTasks)
            where
                malformedTasks = lefts ts
                wellformedTasks = rights ts


handleTask :: (Int, I.Task) -> IO ()
handleTask (i,t) = do
    case calculate t of
        Nothing ->
            putStrLn ("Case #" ++ (show i) ++ ": no solution found")
        Just (i1, i2) ->
            putStrLn ("Case #" ++ (show i) ++ ": " ++ (show i1) ++ " " ++
                      (show i2))
        
    
-- Calculate a single task.
calculate :: I.Task -> Maybe (Int, Int)
calculate t =
    calculate' x xs c
    where
        x = (1, head (I.items t))
        xs = zip [2,3..] (tail (I.items t))
        c = I.credit t
        calculate' _ [] _ = Nothing
        calculate' (i1, x1) xs c =
            -- Look for the first element in xs where the sum matches c
            let l = dropWhile (\(_, x2) -> x1 + x2 /= c) xs in
            case l of
                [] -> calculate' (head xs) (tail xs) c
                ((i2,_):_) -> Just (i1, i2)
