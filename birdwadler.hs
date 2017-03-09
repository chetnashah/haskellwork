

pp xs ys = xs ++ "\n" ++ ys

type Tt = [Char]
type Line = [Char]


myunlines :: [Line] -> Tt
myunlines = foldr pp ""

kkkk = lines.myunlines 

main :: IO()
main = putStrLn "Hi"
