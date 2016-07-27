import System.Environment

memsize :: Int
memsize = 30000
    
allowed :: Char -> Bool
allowed = flip elem "+-<>,.[]"
          
main :: IO ()
main = do
  [path,goal] <- getArgs
  input <- readFile path
  let inputCode = filter allowed input
  if check inputCode
  then do 
    let outputCode = code inputCode
    writeFile goal outputCode
  else error "Mismatch between `[` and `]`"

check :: String -> Bool
check = f 0
    where
      f c [] = c == 0
      f c ('[':xs) = f (c + 1) xs
      f c (']':xs) = if (c > 0) then f (c - 1) xs else False
      f c (_:xs) = f c xs
            
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
generate s@('+':xs) = (if i == 1
                       then "memory[memptr]++;\n" 
                       else "memory[memptr] += " ++ show i ++ ";\n")
                      ++ generate cs
    where (i,cs) = cutPrefix '+' s
generate s@('-':xs) = (if i == 1
                       then "memory[memptr]--;\n"
                       else "memory[memptr] -= " ++ show i ++ ";\n")
                      ++ generate cs
    where (i,cs) = cutPrefix '-' s
generate s@('>':xs) = (if i == 1
                       then "memptr++;\n"
                       else "memptr += " ++ show i ++ ";\n")
                      ++ generate cs
    where (i,cs) = cutPrefix '>' s
generate s@('<':xs) = (if i == 1
                       then "memptr--;\n"
                       else "memptr -= " ++ show i ++ ";\n")
                      ++ generate cs
    where (i,cs) = cutPrefix '<' s
generate (',':xs) = "memory[memptr] = getchar();\n" ++ generate xs
generate ('.':xs) = "putchar(memory[memptr]);\n" ++ generate xs
generate ('[':xs) = "while (memory[memptr] != 0) {\n" ++ generate xs
generate (']':xs) = "}\n" ++ generate xs

cutPrefix :: Char -> String -> (Int,String) -- Int is amount cut, String is leftover
cutPrefix c s@(d:ds) = if d == c
                     then (i + 1, xs)
                     else (0, s)
    where (i,xs) = cutPrefix c ds
cutPrefix _ [] = (0,[])
