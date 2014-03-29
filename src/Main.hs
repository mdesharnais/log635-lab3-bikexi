import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified System.Environment as Environment
import qualified Text.CSV as CSV

import Text.CSV (CSV)

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [path, vector] -> runProgram path vector
        _ -> Environment.getProgName >>= (putStrLn . ("Usage: " ++) . (++ " <file> <query-vector>"))

runProgram :: FilePath -> String -> IO ()
runProgram path vector = do
    eitherErrOrCSV <- CSV.parseCSVFromFile path
    case eitherErrOrCSV of
        (Left err) -> print err
        (Right csv) -> print $ predict (read vector) $ mkHistory $ init csv

type EuclideanVector a = [a]

euclideanNorm :: Floating a => EuclideanVector a -> a
euclideanNorm = sqrt . sum . map (**2)

foo :: CSV.Record -> (Double, Integer)
foo xs = (distance, value)
    where distance = euclideanNorm $ map read (init xs)
          value = read (last xs)

mkHistory :: CSV -> [(Double, Integer)]
mkHistory = map foo

predict :: EuclideanVector Double -> [(Double, Integer)] -> Maybe Integer
predict _ [] = Nothing
predict vector history =
    case lookup norm history of
        Nothing -> Just estimate
        (Just value) -> Just value
    where norm = euclideanNorm vector
          estimate = round $ sum (zipWith (*) ds fs) / sum ds
          ds = take n $ map ((1/) . (**2) . fst) relativeDistances
          fs = take n $ map (fromInteger . snd) relativeDistances
          relativeDistances = List.sort $ map (\(a,b) -> (abs (norm - a), b)) history
          n = length history
