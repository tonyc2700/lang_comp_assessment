-- Anthony S Chapman 51016069
-- CS3518 Lang and Comp Assesment

{--Qestion 1    member "a" ["b","c","a","apple"]   gives True
                member "app" ["b","c","a","apple"]   gives False
        member "a" [1..] gives ERROR - Cannot infer instance

 the question states given a string look for a string
 but it would be simple to change this to change this 
 to look for a char in a list of strings, or an int in
 a list of ints. 
--}
member :: [Char] -> [[Char]] -> Bool
member a [] = False
member a (x:xs)
  |( a == x )= True
  |otherwise = member a xs

{--Question 2    getelt 3 [1,2,3,4,5]   gives Just 3
                 getelt 3 ["b","c","a","apple"] gives Just "a"
 This version handles [] and if number is bigger than the array
 We need Maybe so we can handle [] as [a] could be any type
 It would be easy to change this so it prints a instead of Just a but 
 I wanted you to see what is happening and why I did it like it.--}

--         getelt 2 [1..] gives ERROR - Garbage collection fails to reclaim sufficient space
--                        it happens because length [1..] doesn't terminate.
getelt :: Int -> [a] -> Maybe a
getelt i [] = Nothing
getelt i x = if (length x) < i then Nothing else Just (head ( drop (i-1) x ))

-- This version only handles 'nice' expressions
-- But this version handles infinite sets 
--         getelt 2 [1..] gives 2
getelt2 :: Int -> [a] -> a
getelt2 i x = (head ( drop (i-1) x )) --Maybe some default for [] 

--Queston 3   setelt 2 [3,4,5,6] 10   gives   [3,10,5,6]
--            setelt 2 [1..10] 3 gives [1,3,3,4,5,6,7,8,9,10]
--  infinite  setelt 2 [1..10] 3 gives and infinite list of number but it does replace 2 with 3
--            setelt 2 ["b","c","a","apple"] "cheese" gives ["b","cheese","a","apple"]
--   mixing variables in the list gives an error even though what you want
--   to change is of the same type setelt 2 [1,"c","apple"] "cheese"
setelt :: Int -> [a] -> a -> [a]
setelt i xs x = bef ++ (x : aft)
  where bef = take (i-1) xs
        aft = drop (i) xs

--Question 4    combineWithEach 2 [3,5,7]  gives [-5,-21,-45]
--              combineWithEach 3 [2..8] gives [5,0,-7,-16,-27,-40,-55]
--              combineWithEach 4 [3,5,7]  gives [7,-9,-33]
--              combineWithEach 2 [3,1]  gives [-5,3]
-- Negatives    combineWithEach -1 [3,1]  gives "ERROR - Cannot infer instance" this is due to Int only being positive
-- Char/String  combineWithEach 'a' [3,1]  gives ERROR - Type error in application
-- Infinite     combineWithEach 2 [1..] gives all 
combineWithEach :: Int -> [Int] -> [Int] 
combineWithEach x [] = []
combineWithEach x (y:ys) = ((x+y)*(x-y)):(combineWithEach x ys)

--Question 5    once [2,3,2,4] 2 gives False 
--              once [1..100] 2 gives True
--              once [2] 2 gives True
--              once [2, "d"] 2 gives ERROR - Type error in application
--              once [2, 2] "s" gives ERROR - Type error in application
-- isIn makes a list of elements equal to n.
--   infinite   once [1..] 2 carries on working forever
isIn:: [Int] -> Int -> [Int]
isIn y n =  [x | x <- y, n == x] -- greates a list consiting of n out of a list. 
--if the element isn't it the list, isIn = [] implies length(isIn) = 0 which /= 1
--if there are multiples then length(isIn) > 1 
once :: [Int] -> Int -> Bool
once [] x = False
once y n = (length(isIn y n) == 1)

--TicTaToe

--type "showBoard board" to see my example of such a board
--to inspect each row, just type "row1" , "row2" or "row3" into Hugs

--
data Move = X | O | N deriving (Eq, Show, Read)
-- X is a cross, O is a nought and N is empty (Nill)
type Row = [Move] -- A Row is a list of Moves
type Board = [Row] -- A Board is a list of Rows
-- This makes Board a list of lists of Moves. 

--You can create more rows and/or add more 'Move's to each row.
--But then it wouldn't be Noughts and crosses..!
row1 :: Row 
row1 = [N, N, N]
row2 :: Ro
row2 = [N, N, N]
row3 :: Row
row3 = [N, N, N]
--I've created board to be full of N (ie. empty)
board :: Board
board = [row1,row2,row3]
{--board1 = nicer board (visiually only, they're the same board...)
  well, not the same board but board1 is a board GUI.
  type board1 on Hugs to see the board--}
board1 = showBoard board
-- this return every element of the row and turns it into a string
--it's good so you can inspect a row and makes it able to manipulate
showRow :: [Move] -> String
showRow [a, b, c] = show a ++ " | " ++ show b ++ " | " ++ show c

-- given a board it returns it in a logical to see it only Hugs.
showBoard :: Board -> IO()
showBoard [a, b, c] = putStrLn (showRow a ++ "\n"
                                ++ showRow b ++ "\n"
                                ++ showRow c)

--similarly to question 3 changeCol changes the column of a given row

{-- I decided to rename the function istead of calling setelt
    as it will make more sense when reading the code of the function
    move. It does mean more lines of code but the whole point of 
    re-defining datatypes is to make the code more user-friendly. 
    Hence if we go through the trouble of defing Move, Row and Board, 
    then changeCol and getRow make more sense to read.  --}
changeCol :: Row -> Int -> Move -> Row
changeCol c i m = bef ++ (m : aft)
  where bef = take (i-1) c 
        aft = drop (i) c

--similar to question 2 this selects a chosen row of a given board

{-- I decided to re-type this next fuction instead of just re-using getelt
    because all inputs are defined. Of course if someone tried to get a row
    which isn't there, then it would crash. --}
getRow :: Board -> Int -> Row
getRow b r = head (drop (r-1) b)

--given a (Board row column Move) it selects the row, changes the column
--of that row and replaces the old row with the new one into the board.
--ROWS AND COLUMNS START AT 1 NOT 0!
move :: Board -> Int -> Int-> Move -> Board
move b 0 _ _ = b --Rows and colums begin at 1
move b _ 0 _ = b --Rows and colums begin at 1
move b r c m = bef ++ ((changeCol(getRow b r) c m) : aft)
  where bef = take (r-1) b
        aft = drop (r) b
