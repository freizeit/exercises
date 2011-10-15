module Input
( parseFile
, Task(..)
) where


import Data.Either
import qualified Data.ByteString.Lazy.Char8 as L


data Task = Task { credit :: Int
                 , items :: [Int] } deriving (Show)


-- Parse an input file.
parseFile :: String -> IO (Maybe [Either String Task])
parseFile name = do
    -- ignore first line in the file
    contents <- L.readFile name
    let lines = L.split '\n' contents
        -- ignore the first (total number of tasks) and the last (empty) line.
        theTasks = tasks $ (tail . init) lines
    return theTasks


-- Takes a sequence of input lines and returns either an error (if the input
-- file was malformed) or a list of tasks.
tasks :: [L.ByteString] -> Maybe [Either String Task]
tasks lines =
    tasks' lines []
    where
        tasks' [] acc = Just (reverse acc)
        tasks' (c:_:is:lines) acc = tasks' lines ((task c is):acc)
        tasks' _ _ = Nothing


-- Takes a credit and an item list and returns either an error (if either of
-- the inputs was malformed) or a task
task :: L.ByteString -> L.ByteString -> Either String Task
task credit items =
    case L.readInt credit of
        Just (cval, _) -> case extractItems items of
            Left error -> Left error
            Right ivals -> Right (Task cval ivals)
        Nothing -> Left $ "invalid credit value '" ++ show credit ++ "'"
            

-- Extract a list of integers.
extractItems :: L.ByteString -> Either String [Int]
extractItems is =
    items' (L.split ' ' is) []
    where
        items' [] acc = Right (reverse acc)
        items' (i:is) acc =
            case L.readInt i of
                Just (ival,_) -> items' is (ival:acc)
                Nothing -> Left $ "invalid item value '" ++ show i ++ "'"
