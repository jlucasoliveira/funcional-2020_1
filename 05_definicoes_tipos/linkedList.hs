data LinkedList a = Vazia | No a (LinkedList a) deriving (Eq, Show)

fromList :: [a] -> LinkedList a
fromList [] = Vazia
fromList (x:xs) = No x (fromList xs)

toList :: LinkedList a -> [a]
toList Vazia = []
toList (No a b) = a:(toList b)

append :: a -> LinkedList a -> LinkedList a
append a Vazia = No a Vazia
append a (No n b) = No n (append a b)

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList a = fromList $ reverse $ toList a