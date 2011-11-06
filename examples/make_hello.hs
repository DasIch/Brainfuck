import System.Environment (getArgs)
import Data.Char (ord)
import Data.List (intercalate)

main :: IO ()
main = do
    args <- getArgs
    if length args < 2
       then putStrLn "Requires at least two arguments"
       else return ()
    let outFileName = args !! 0
    let string = unwords $ drop 1 args
    writeFile outFileName $ generate string


generate :: String -> String
generate xs = intercalate "." (map compile (asDifferences $ map ord xs)) ++ "."
    where compile 0 = ""
          compile n = if n < 0
                         then replicate (abs n) '-'
                         else replicate n '+'


asDifferences :: Num a => [a] -> [a]
asDifferences (x:xs) = reverse . fst $ foldl step ([x], x) xs
    where step (result, previous) next = (next - previous : result, next)
asDifferences xs = xs
