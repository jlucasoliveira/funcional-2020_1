data Trem a = Vagao a ( Trem a ) | Vazio deriving Show
type Quantidade = Int
type Peso = Int
data Carga = SemCarga | Passageiro Quantidade | Mercadoria Peso deriving Show

numPassageiros :: Trem Carga -> Int
numPassageiros Vazio = 0
numPassageiros (Vagao SemCarga a) = numPassageiros a
numPassageiros (Vagao (Mercadoria _) a) = numPassageiros a
numPassageiros (Vagao (Passageiro q) a) = q + numPassageiros a
