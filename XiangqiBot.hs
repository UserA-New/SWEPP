-- module (NICHT ÄNDERN!)
module XiangqiBot
    ( getMove
    , listMoves
    ) 
    where

import Data.Char
-- More modules may be imported

import Util

--- external signatures (NICHT ÄNDERN!)
getMove :: String -> String
getMove = _getMoveImpl_ -- YOUR IMPLEMENTATION HERE

listMoves :: String -> String
listMoves = _listMovesImpl_ -- YOUR IMPLEMENTATION HERE


-- YOUR IMPLEMENTATION FOLLOWS HERE

{-

    Der Startzustand des Spiels ist der Folgende:
        "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR"

    Xiangqi:

    a9 -- b9 -- c9 -- d9 -- e9 -- f9 -- g9 -- h9 -- i9
    81 -- 82 -- 83 -- 84 -- 85 -- 86 -- 87 -- 88 -- 89

    a8 -- b8 -- c8 -- d8 -- e8 -- f8 -- g8 -- h8 -- i8
    72 -- 73 -- 74 -- 75 -- 76 -- 77 -- 78 -- 79 -- 80

    a7 -- b7 -- c7 -- d7 -- e7 -- f7 -- g7 -- h7 -- i7
    63 -- 64 -- 65 -- 66 -- 67 -- 68 -- 69 -- 70 -- 71

    a6 -- b6 -- c6 -- d6 -- e6 -- f6 -- g6 -- h6 -- i6
    54 -- 55 -- 56 -- 57 -- 58 -- 59 -- 60 -- 61 -- 62

    a5 -- b5 -- c5 -- d5 -- e5 -- f5 -- g5 -- h5 -- i5
    45 -- 46 -- 47 -- 48 -- 49 -- 50 -- 51 -- 52 -- 53
       --    --    --    --    --    --    --    --   
       --    --    --    --    --    --    --    --   (River)
       --    --    --    --    --    --    --    --
    a4 -- b4 -- c4 -- d4 -- e4 -- f4 -- g4 -- h4 -- i4
    36 -- 37 -- 38 -- 39 -- 40 -- 41 -- 42 -- 43 -- 44

    a3 -- b3 -- c3 -- d3 -- e3 -- f3 -- g3 -- h3 -- i3
    27 -- 28 -- 29 -- 30 -- 31 -- 32 -- 33 -- 34 -- 35

    a2 -- b2 -- c2 -- d2 -- e2 -- f2 -- g2 -- h2 -- i2
    18 -- 19 -- 20 -- 21 -- 22 -- 23 -- 24 -- 25 -- 26

    a1 -- b1 -- c1 -- d1 -- e1 -- f1 -- g1 -- h1 -- i1
    9  -- 10 -- 11 -- 12 -- 13 -- 14 -- 15 -- 16 -- 17

    a0 -- b0 -- c0 -- d0 -- e0 -- f0 -- g0 -- h0 -- i0
    0  -- 1  -- 2  -- 3  -- 4  -- 5  -- 6  -- 7  -- 8
-}

{-
    Miscellaneous
-}

{-
    The function converts a whole number greater than 1
    into a series of 1s corresponding to that number in a string!
-}
representedByOnes :: Char -> [Char]
representedByOnes charNum
    | charNum == '9' = "111111111"
    | charNum == '8' = "11111111"
    | charNum == '7' = "1111111"
    | charNum == '6' = "111111"
    | charNum == '5' = "11111"
    | charNum == '4' = "1111"
    | charNum == '3' = "111"
    | charNum == '2' = "11"
    | otherwise = [charNum]

{-
    The function returns a string, 
    in which whole numbers are converted into a series of 1s corresponding to that number.
-}
fensWOnes :: [Char] -> [Char]
fensWOnes fens = concat(map representedByOnes fens)

{-
    In order to use the (mod 9), we have to get rid of forward slashes!
    The Fuction return a string with the forward slashes in it.
-}
fensWoFs :: [Char] -> [Char]
fensWoFs fens = concat(splitOn '/' fens)

{-
    Index positions of a string to which the functions fensWOnes and fensWoFs are applied are the following:
    90 -> whitespace    
    91 -> shows the player whose turn it's.

    The function converts the given index into a position of form [Column] [Row], i.e c3, d7, e2, etc.
-}
whereIsIt :: Int -> [Char]
whereIsIt index = (letterPart index) ++ (numberPart index)

{-
    The function extract the last character of a string, that means it returns the next player.
-}
whoseNext :: [Char] -> Char
whoseNext fens = last fens


{-
    The function returns the column of a position.
-}
letterPart :: Int -> [Char]
letterPart index
    | index <= 89 =
        case () of
            () | index `mod` 9 == 0 -> "a"
            () | index `mod` 9 == 1 -> "b"
            () | index `mod` 9 == 2 -> "c"
            () | index `mod` 9 == 3 -> "d"
            () | index `mod` 9 == 4 -> "e"
            () | index `mod` 9 == 5 -> "f"
            () | index `mod` 9 == 6 -> "g"
            () | index `mod` 9 == 7 -> "h"
            () | index `mod` 9 == 8 -> "i"
    | otherwise = "Error! The index is greater than 89!"

{-
    The function returns the row of a position.
-}
numberPart :: Int -> [Char]
numberPart index 
    | index <= 89 =
        case () of
            () | index `div` 9 == 0 -> "0"
               | index `div` 9 == 1 -> "1"
               | index `div` 9 == 2 -> "2"
               | index `div` 9 == 3 -> "3"
               | index `div` 9 == 4 -> "4"
               | index `div` 9 == 5 -> "5"
               | index `div` 9 == 6 -> "6"
               | index `div` 9 == 7 -> "7"
               | index `div` 9 == 8 -> "8"
               | index `div` 9 == 9 -> "9"
    | otherwise = "Error! The index is greater than 89!"

-- IMPLEMENT FROM HERE