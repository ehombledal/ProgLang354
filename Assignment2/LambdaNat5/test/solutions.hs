--MEMBER--
isMember :: Int -> [Int] -> Int
isMember _ [] = 0
isMember x (y : ys) = if x == y 
    then 1 
    else isMember x ys 

--REMOVE--
toRemove :: Int -> [Int] -> [Int]
toRemove x [] = []
toRemove x (y:ys) = if x == y
    then ys
    else y : toRemove x ys

--SUM--
sumList :: [Int] -> Int
sumList [] = 0
sumList y = sum (y)

--PROD--
prod :: [Int] -> Int
prod [] = 0
prod y = product (y)

--PLUSTWO--
plustwo :: Int -> Int
plustwo x = x + 2;

--MAP--
toMap :: Int -> [Int] -> [Int]
toMap x [] = []
toMap x (y : ys) = y + x : toMap x (ys)


--INSERT--
insertList :: Int -> [Int] -> [Int]
insertList x [] = [x]
insertList x (y:ys) = if x < y
    then x : y : ys
    else y : insertList x ys 


--SORT--
sortList :: [Int] -> [Int]
sortList [y] = [y]
sortList (y : ys) = insertList y (sortList ys)

main = do
    let list1 = [2,3,1]
    print $ isMember 1 (list1)

    let list2 = [1,2,3,1,2,3]
    print $ toRemove 2 (list2);

    let list3 = [1,2,3,4]
    print $ sumList (list3)
    print $ prod (list3)


    --print $ plustwo (3);
    print $ toMap (plustwo 0) list3 --this effectively adds 2

    --print $ insertList 6 list3
    print $ sortList list2

