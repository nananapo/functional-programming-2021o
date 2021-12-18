data Stack a = Empty | Push a (Stack a)

top::Stack a -> a
top (Push x _) = x

pop::Stack a -> Stack a
pop (Push _ s) = s
