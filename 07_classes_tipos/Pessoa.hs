import Data.List

data Pessoa = Pessoa {nome :: String, idade :: Int, salario :: Float}

data Criterio = ByNome | ByIdade | BySalario

-- classifica lista de pessoa por critério
sortListPessoa :: [Pessoa] -> Criterio -> [Pessoa]
sortListPessoa p ByNome = sortBy (\a b -> compare (nome a) (nome b)) p
sortListPessoa p ByIdade = sortBy (\a b -> compare (idade a) (idade b)) p
sortListPessoa p BySalario = sortBy (\a b -> compare (salario a) (salario b)) p

instance Show Pessoa where
    show p = show (nome p) ++ " tem " ++ show (idade p) ++ " anos e ganha de salario " ++ show (salario p)