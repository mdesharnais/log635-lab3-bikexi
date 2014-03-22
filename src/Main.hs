import qualified Text.CSV as CSV
import qualified System.Environment as Environment

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [path] -> runProgram path
        _ -> showErrorMessage

runProgram :: FilePath -> IO ()
runProgram path = do
    eitherErrOrCSV <- CSV.parseCSVFromFile path
    case eitherErrOrCSV of
        (Left err) -> print err
        (Right csv) -> putStrLn $ CSV.printCSV csv

showErrorMessage :: IO ()
showErrorMessage = do
    progName <- Environment.getProgName
    putStrLn $ "Usage: " ++ progName ++ " <file>"
