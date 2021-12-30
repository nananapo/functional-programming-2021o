data Maybee a = Noothing | Juust a    deriving (Eq,Ord,Show)

loookup::(Eq a) => a -> [(a,b)] -> Maybee b
loookup _ [] = Noothing
loookup t (c:cs) = if t == fst c then Juust (snd c) else loookup t cs