main :: IO () -- Lines 1-4 inspiration taken from the solution to kattis problem
main = do
    inpt <- getLine
    let n = read inpt
    let lst = [4, 5, 7, 4, 2, 7, 9, 2] -- Improvised list instead of user input
        m = if even n
            then div (n+1) 2
            else  div n 2
        sortedlist = recsort lst
        top = take m (reverse sortedlist)
        res = sum top
    print res

recsort :: [Int] -> [Int] -- Recursive sorting function (acending order) aka quicksort
recsort [] = [] -- Base case
recsort (x:xs) = recsort [y | y <- xs, y < x] ++ [x] ++ recsort [y | y <- xs, y >= x] 
-- To the left all numbers lower than x
-- To the right all numbers greater or eaual to x
-- In the middle x