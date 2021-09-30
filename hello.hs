main = do
    print "My first Haskell Program"
    name <- getLine
    print ("Hello, " ++ name)

f x y = x + y 
g = \x y -> x * y * min x y 