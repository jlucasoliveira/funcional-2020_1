data Arv a = Vazia | No a ( Arv a ) ( Arv a ) deriving (Eq , Show)

insertArvore :: Ord a => a -> Arv a -> Arv a
insertArvore x Vazia = (No x Vazia Vazia)
insertArvore x (No a esq dir) = if x <= a then (No a (insertArvore x esq) dir) else (No a esq (insertArvore x dir))

data Arvore a = Folha a | Ramo (Arvore a) (Arvore a) deriving (Show)

foldTree :: (a->b) -> (b->b->b) -> Arvore a -> b
foldTree f _ (Folha a) = f a
foldTree f g (Ramo a b) = g (foldTree f g a) (foldTree f g b)

menorNivelFolha :: Arv a -> [a]
menorNivelFolha Vazia = []

removeFolhas :: Arv a -> Arv a
removeFolhas Vazia = Vazia
removeFolhas (No a Vazia Vazia) = Vazia
removeFolhas (No a b c) = (No a (removeFolhas b) (removeFolhas c))

cheia :: Arv a -> Bool
cheia (No _ (No _ _ _) Vazia) = False
cheia (No _ Vazia (No _ _ _)) = False
cheia (No _ Vazia Vazia) = True
cheia (No _ a b) = cheia a && cheia b
