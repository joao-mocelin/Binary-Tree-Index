import Data.Char -- necessária para import a função isPunctuation e toLower
import System.IO --necessária para função hFlush, que limpa o buffer para a entrada do usuario.
type Doc = String
type Line = String
type Word' = String
data Tree = Node Int [Word'] Tree Tree | Leaf deriving Show

main :: IO ()
main = do
    putStrLn("Qual o nome do arquivo a ser lido? ")
    hFlush stdout
    arquivo <- getLine
    texto <- readFile arquivo
    let arvore = makeIndexTree texto
    print arvore

toLowerString :: String -> String
toLowerString = map toLower -- aplica toLower a cada char da string

removePunctuation [] = [] -- remove as pontuações do texto, que irão interferir na checagem de palavras.
removePunctuation (x:xs) = if isPunctuation x then removePunctuation xs else x : removePunctuation xs

numLines :: [Line] -> [(Int,Line)] --recebe uma lista de strings e retorna uma lista de tuplas (numero da linha, texto da linha)
numLines xs = numLines' 1 xs
numLines' _ [] = []
numLines' n (x:xs) = [(n, x)] ++ numLines' (n+1) xs


allNumWords :: [(Int,Line)] -> [(Int,Word')] -- recebe uma lista de tuplas (numero da linha, texto da linha) e separa em uma lista de tuplas (numero da linha, palavra)
-- a separação por palavras ocorre com a função words.
allNumWords ([]) = []
allNumWords ((x,y):xs) = allNumWords' x y ++ allNumWords xs
allNumWords' n xs = [(n, word) | word <- words xs]



insOrd :: (Eq a, Ord a) => a -> [a] -> [a]
insOrd x [] = [x]
insOrd x (y:ys)
    | x < y     = x : y : ys
    | x == y    = y : ys
    | otherwise = y : insOrd x ys

ins :: (Int, Word') -> Tree -> Tree
ins (linha, palavra) Leaf = Node linha [palavra] Leaf Leaf
ins (linha, palavra) (Node l pls esq dir) | linha == l = Node l (insOrd palavra pls) esq dir
                                          | linha < l = Node l pls (ins (linha, palavra) esq) dir
                                          | otherwise = Node l pls esq (ins (linha, palavra) dir)

mIndexTree :: [(Int, Word')] -> Tree
mIndexTree ls = foldl (mIndexTree') Leaf ls -- foldl recebe uma função , um acumulador (Leaf = nó vazio) e um elemento que vem de (allNumWords) que é uma lista de tuplas (linha, palavra).
mIndexTree' tree (linha,palavra) = ins (linha,palavra) tree -- inverte a ordem dos argumentos

makeIndexTree :: Doc -> Tree
makeIndexTree doc = mIndexTree . allNumWords . numLines . lines . removePunctuation . toLowerString $ doc
