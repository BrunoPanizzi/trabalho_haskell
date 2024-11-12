-- Integrantes: Bruno Panizzi, Enzo Vivian, Gustavo Amaro, Natan Dias 

import System.IO
import Text.Read (readMaybe)
import System.Exit (exitFailure)
import Data.List (groupBy)

-- definir o tipo Item
data Item = Item {
  nome :: String, 
  preco :: Int, 
  duracao :: Int
} deriving Show

main :: IO ()
main = do
    putStrLn "Bem-vindo ao Kit de Sobrevivência Espacial!"
    menu []

-- string to int
strToInt :: String -> IO Int
strToInt s = case readMaybe s of
    Just n  -> return n
    Nothing -> do
        putStrLn "Erro: Número inválido"
        exitFailure

printItemList :: [Item] -> IO [()]
printItemList itens = 
    mapM (\(i, item) -> putStrLn (show i ++ ". " ++ show item)) (zip [1..] itens)

-- Menu
menu :: [Item] -> IO ()
menu itens = do
    putStrLn "\nMenu Principal:"
    putStrLn "1. Adicionar item ao kit"
    putStrLn "2. Listar todos os itens no kit"
    putStrLn "3. Remover item do kit"
    putStrLn "4. Calcular custo total do kit"
    putStrLn "5. Calcular duração do kit"
    putStrLn "6. Sair da aplicação"
    putStr "Escolha uma opção: ";
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> adicionarItem itens
        "2" -> listarItens itens
        "3" -> removerItem itens
        "4" -> calcularCusto itens
        "5" -> calcularDuracao itens
        "6" -> putStrLn "Encerrando a aplicação. Até logo!"
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            menu itens

-- Adiciona item ao kit
adicionarItem :: [Item] -> IO ()
adicionarItem itens = do
    putStr "Nome do item: ";
    hFlush stdout
    nome <- getLine
    putStr "Preço do item: ";
    hFlush stdout
    precoStr <- getLine
    preco <- strToInt precoStr
    -- TODO: não crashar o app
    putStr "Duração do item (dias): ";
    hFlush stdout
    duracaoStr <- getLine
    duracao <- strToInt duracaoStr
    let novoItem = Item nome preco duracao
    putStrLn "Item adicionado com sucesso!"
    menu (novoItem : itens)

-- Listar todos os itens no kit
listarItens :: [Item] -> IO ()
listarItens [] = do
    putStrLn "O kit está vazio."
    menu []
listarItens itens = do
    putStrLn "Itens no kit:"
    printItemList itens
    menu itens

-- Remover um item do kit
removerItem :: [Item] -> IO ()
removerItem [] = do
    putStrLn "O kit está vazio. Nada para remover."
    menu []
removerItem itens = do
    putStrLn "Itens no kit:"
    printItemList itens
    putStr "Digite o número do item a ser removido: ";
    hFlush stdout
    indiceStr <- getLine
    let indice = read indiceStr :: Int
    if indice > 0 && indice <= length itens
        then do
            let novosItens = take (indice - 1) itens ++ drop indice itens
            putStrLn "Item removido com sucesso!"
            menu novosItens
        else do
            putStrLn "Índice inválido. Tente novamente."
            removerItem itens

-- Custo total do kit
calcularCusto :: [Item] -> IO ()
calcularCusto itens = do
    let total = sum (map preco itens)
    putStrLn ("Custo total do kit: " ++ show total ++ " pilas")
    menu itens

-- Duração dos itens do kit
--
-- O cálculo da duração assume que todos os 
-- itens são consumidos em paralelo, exceto 
-- pelos itens com o mesmo nome, que tem suas
-- durações somadas
--
-- Ex: 
-- calcularDuracao [ { água, 2, 1 }, { barraca, 100, 365 } ]
-- deve retornar 1, pois a água acaba antes da barraca
calcularDuracao :: [Item] -> IO ()
calcularDuracao itens = do
    let totalDuracao = minimum $ map (\group -> duracao (head group) * length group) (groupBy (\a b -> nome a == nome b) itens)
    putStrLn ("Duração total do kit: " ++ show totalDuracao ++ " dias")
    menu itens
