{-| A program that, based on some historic data in a CSV file, use the <http://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm kNN algorithm> to predict the output of a given vector.

The CSV file must have the following properties:

 * it does not have a header line

 * each line correspond to a vector of data

 * on a given line, the last cell correspond to the output and the others to the input vector

Here is an example of a well-formed CSV file:

> 1,2,3,4,50
> 6,7,8,9,10
> 2,3,4,5,60
> 7,8,9,1,20
> 3,4,5,6,70

The vector must be a string representing a list of real number values, e.g.

> "[1,3,5,7]"

Here is an example use of the command.

>>> log635-lab3-bikexi data/foo.csv "[2,4,6,8]"
Just 54
-}
module Main where

import qualified Data.List as List
import qualified System.Environment as Environment
import qualified Text.CSV as CSV

import Control.Applicative ((<|>))
import Text.CSV (CSV)

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [path, vector] -> runProgram path vector
        _ -> Environment.getProgName >>= putStrLn . ("Usage: " ++) . (++ " <file> <query-vector>")

{-| Learn from some data in a CSV file and predict the ouput of the given vector.  -}
runProgram :: FilePath -- ^ Relative path to the CSV file containing the history from which the system will learn.
           -> String   -- ^ String representation of the vector to predict the output value.
           -> IO ()
runProgram path vector = do
    eitherErrOrCSV <- CSV.parseCSVFromFile path
    case eitherErrOrCSV of
        (Left err) -> print err
        (Right csv) -> print $ predict (read vector) $ mkHistory $ init csv

type EuclideanVector a = [a]

{-| Calculate the euclidean norm of a given vector.

> euclideanNorm [a,b,c] == sqrt (a**2 + b**2 + c**2)
-}
euclideanNorm :: Floating a => EuclideanVector a -> a
euclideanNorm = sqrt . sum . map (**2)

{-| Convert the data read from the csv file to a list of euclidean norm and output value.  -}
mkHistory :: CSV -> [(Double, Integer)]
mkHistory = map f
    where f xs = (distance, value)
              where distance = euclideanNorm $ map read (init xs)
                    value = read (last xs)

{-| Predict the output value of a given vector, based on the history of other vectors.  -}
predict :: EuclideanVector Double -> [(Double, Integer)] -> Maybe Integer
predict _ [] = Nothing
predict vector history = lookup norm history <|> Just estimate
    where norm = euclideanNorm vector
          estimate = round $ sum (zipWith (*) ds fs) / sum ds
          (ds, fs) = unzip $ take 5 $ List.sortBy (flip compare) $ map d history
          d (a, b) = (1 / (norm - a)**2, fromInteger b)
