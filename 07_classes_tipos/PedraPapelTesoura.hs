data Gesto = Pedra | Papel | Tesoura

ganhaDe :: Gesto -> Gesto -> Bool
ganhaDe Pedra Tesoura = True
ganhaDe Papel Pedra = True
ganhaDe Tesoura Papel = True
ganhaDe _ _ = False

ganhadores :: [(Gesto,Gesto)] -> [Int]
ganhadores xs = [y | ((xg1, xg2),y) <- zip xs [0..], ganhaDe xg1 xg2]