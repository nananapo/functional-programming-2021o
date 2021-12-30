replicatee::Int -> a -> [a]
replicatee 0 a = []
replicatee b a = a:(replicatee (b-1) a)