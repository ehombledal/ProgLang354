--MEMBER--
isMember _ [] = 0
isMember x (y : ys) = if x == y 
    then 1 
    else isMember x ys 

--REMOVE--
toRemove _ [] = []
toRemove x (y : ys) = if x == y
    then []
    else toRemove x ys


--SUM--
sumList [] = 0
sumList y = sum (y)

--PROD--
prod [] = 0
prod y = product (y)

--PLUSTWO--

--MAP--

--INSERT--

--SORT--

main = do
    let list = [1,2,3,4]
    print $ isMember 4 (list)

    --print $ toRemove 3 (list);
    print $ product (list)
    print $ sumList (list)

