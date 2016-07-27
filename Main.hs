import System.Environment

allowed :: Char -> Bool
allowed = (flip elem) "+-<>,.[]"
          
main :: IO ()
main = do
  (path:_) <- getArgs
  input <- readFile path
  let inputCode = filter allowed input
  putStrLn inputCode
