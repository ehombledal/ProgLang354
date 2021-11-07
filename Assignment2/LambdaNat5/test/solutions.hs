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
toMap x (y) = toMap x (y + x:ys)


--INSERT--
insertList :: Int -> [Int] -> [Int]
insertList x [] = []
insertList x y = x : y --adds to front of list

--SORT--
sortList :: [Int] -> [Int]
sortList [] = []
--sortList y = 

main = do
    let list1 = [2,3,1]
    print $ isMember 1 (list1)

    let list2 = [1,2,3,1,2,3]
    print $ toRemove 2 (list2);

    let list3 = [1,2,3,4]
    print $ prod (list3)
    print $ sumList (list3)

    print $ plustwo (3);
    print $ toMap plustwo list3

    --print $ insertList 3 list3

