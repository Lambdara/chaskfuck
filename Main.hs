import System.Environment

memsize :: Int
memsize = 30000
    
allowed :: Char -> Bool
allowed = (flip elem) "+-<>,.[]"
          
main :: IO ()
main = do
  (path:goal:[]) <- getArgs
  input <- readFile path
  let inputCode = filter allowed input
  let outputCode = code inputCode
  writeFile goal outputCode

code :: String -> String
code input =
    "#include <stdio.h>\n" ++
    "int memory[" ++ show memsize ++ "];\n" ++
    "int memptr=0;\n" ++
    "int main(void) {\n" ++
    generate input ++
    "}\n"

generate :: String -> String
generate [] = ""
generate ('+':xs) = "memory[memptr] += 1;\n" ++ generate xs
generate ('-':xs) = "memory[memptr] -= 1;\n" ++ generate xs
generate ('>':xs) = "memptr++;\n" ++ generate xs
generate ('<':xs) = "memptr--;\n" ++ generate xs
generate (',':xs) = "memory[memptr] = getchar();\n" ++ generate xs
generate ('.':xs) = "putchar(memory[memptr]);\n" ++ generate xs
generate ('[':xs) = "while (memory[memptr] != 0) {\n" ++ generate xs
generate (']':xs) = "}\n" ++ generate xs
